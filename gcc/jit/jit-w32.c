/* Functions used by the Windows port of libgccjit.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Nicolas Bertolo <nicolasbertolo@gmail.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"

/* Required for rand_s */
#define _CRT_RAND_S

#include <cstdio>
#include <cstdint>

#include "jit-w32.h"

#include "libiberty.h"

#include <accctrl.h>
#include <aclapi.h>

namespace gcc {
namespace jit {
void
print_last_error (void)
{
  LPSTR psz = NULL;
  DWORD dwErrorCode;
  dwErrorCode = GetLastError();
  const DWORD cchMsg = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM
                                     | FORMAT_MESSAGE_IGNORE_INSERTS
                                     | FORMAT_MESSAGE_ALLOCATE_BUFFER
                                     | FORMAT_MESSAGE_MAX_WIDTH_MASK,
                                     NULL,
                                     dwErrorCode,
                                     MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                                     reinterpret_cast<LPSTR>(&psz),
                                     0,
                                     NULL);
  if (cchMsg > 0)
    {
      fprintf (stderr, "%s\n", psz);
      LocalFree (psz);
    }
  else
    {
      fprintf (stderr, "Failed to retrieve error message string for error %lu\n",
               dwErrorCode);
    }
}

/* Helper function used for getting the SID belonging to the current user. */
static TOKEN_USER*
get_TOKEN_USER_current_user ()
{
  TOKEN_USER *result = NULL;

  HANDLE process_token = INVALID_HANDLE_VALUE;

  DWORD token_user_info_len;
  TOKEN_USER *token_user = NULL;

  /* Get current process access token. */
  if (!OpenProcessToken (GetCurrentProcess (), TOKEN_READ,
                         &process_token))
    return NULL;

  /* Get necessary buffer size. */
  if (!GetTokenInformation(process_token, TokenUser, NULL, 0, &token_user_info_len)
      && GetLastError() != ERROR_INSUFFICIENT_BUFFER)
    goto cleanup;

  token_user = (TOKEN_USER*) new char[token_user_info_len];

  /* Get info about the user of the process */
  if (!GetTokenInformation (process_token, TokenUser, token_user,
                            token_user_info_len, &token_user_info_len))
      goto cleanup;

  result = token_user;

 cleanup:
  if (process_token != INVALID_HANDLE_VALUE)
    CloseHandle(process_token);

  if (token_user != NULL && result == NULL)
    delete[] (char*)token_user;

  return result;
}

/* Helper function to create a directory with permissions such that only the
  current user can access it. */
static bool
create_directory_for_current_user (const char * path)
{
  PACL pACL = NULL;
  EXPLICIT_ACCESS ea;
  SECURITY_ATTRIBUTES sa;
  SECURITY_DESCRIPTOR SD;
  DWORD dwRes;
  bool result = true;
  TOKEN_USER *token_user = NULL;

  token_user = get_TOKEN_USER_current_user();
  if (!token_user)
    return false;

  memset (&ea, 0, sizeof (EXPLICIT_ACCESS));
  ea.grfAccessPermissions = GENERIC_ALL; /* Access to all. */
  ea.grfAccessMode = SET_ACCESS; /* Set access and revoke everything else. */
  /* This is necessary for the Windows Explorer GUI to show the correct tick
     boxes in the "Security" tab. */
  ea.grfInheritance = OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE;
  ea.Trustee.TrusteeForm = TRUSTEE_IS_SID;
  ea.Trustee.TrusteeType = TRUSTEE_IS_USER;
  ea.Trustee.ptstrName = (char*) token_user->User.Sid;

  /* Create a new ACL that contains the new ACEs. */
  dwRes = SetEntriesInAcl(1, &ea, NULL, &pACL);
  if (dwRes != ERROR_SUCCESS)
    return false;

  if (!InitializeSecurityDescriptor (&SD,
                                     SECURITY_DESCRIPTOR_REVISION))
    goto cleanup;

  /* Add the ACL to the security descriptor. */
  if (!SetSecurityDescriptorDacl (&SD,
                                  TRUE,     /* use pACL */
                                  pACL,
                                  FALSE))   /* not a default DACL */
    goto cleanup;

  /* Initialize a security attributes structure. */
  sa.nLength = sizeof (SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = &SD;
  sa.bInheritHandle = FALSE;

  /* Finally create the directory */
  if (!CreateDirectoryA (path, &sa))
    result = false;

 cleanup:
  if (pACL)
    LocalFree (pACL);

  if (token_user)
    delete[] (char*)token_user;

  return result;
}


char *
win_mkdtemp (void)
{
  char lpTempPathBuffer[MAX_PATH];

  /* Gets the temp path env string (no guarantee it's a valid path). */
  DWORD dwRetVal = GetTempPath (MAX_PATH, lpTempPathBuffer);
  if (dwRetVal > MAX_PATH || (dwRetVal == 0))
    {
      print_last_error ();
      return NULL;
    }

  /* Check that the directory actually exists. */
  DWORD dwAttrib = GetFileAttributes (lpTempPathBuffer);
  bool temp_path_exists = (dwAttrib != INVALID_FILE_ATTRIBUTES
                           && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
  if (!temp_path_exists)
    {
      fprintf (stderr, "Path returned by GetTempPath does not exist: %s\n",
               lpTempPathBuffer);
    }

  /* Make sure there is enough space in the buffer for the prefix and random
     number.*/
  int temp_path_buffer_len = dwRetVal;
  const int appended_len = strlen ("\\libgccjit-123456");
  if (temp_path_buffer_len + appended_len + 1 >= MAX_PATH)
    {
      fprintf (stderr, "Temporary file path too long for generation of random"
               " directories: %s", lpTempPathBuffer);
    }

  /* This is all the space we have in the buffer to store the random number and
     prefix. */
  int extraspace = MAX_PATH - temp_path_buffer_len - 1;

  int tries;
  const int max_tries = 1000;

  for (tries = 0; tries < max_tries; ++tries)
    {
      /* Get a random number in [0; UINT_MAX]. */
      unsigned int rand_num;
      if (rand_s (&rand_num) != 0)
        {
          fprintf (stderr,
                   "Failed to create a random number using rand_s(): %s\n",
                   _strerror (NULL));
          return NULL;
        }

      /* Create 6 digits random number. */
      rand_num = ((double)rand_num / ((double) UINT_MAX + 1 ) * 1000000);

      /* Copy the prefix and random number to the buffer. */
      snprintf (&lpTempPathBuffer[temp_path_buffer_len], extraspace,
                "\\libgccjit-%06u", rand_num);

      if (create_directory_for_current_user (lpTempPathBuffer))
        break; // success!

      /* If we can't create the directory because we got unlucky and the
         directory already exists retry, otherwise fail. */
      if (GetLastError () != ERROR_ALREADY_EXISTS)
        {
          print_last_error ();
          return NULL;
        }
    }

  if (tries == max_tries)
    {
      fprintf (stderr, "Failed to create a random directory in %s\n",
               lpTempPathBuffer);
      return NULL;
    }

  {
    int allocate_len = temp_path_buffer_len + appended_len + 1;
    char * result = XNEWVEC (char, allocate_len);
    strcpy (result, lpTempPathBuffer);
    return result;
  }
}
}
}
