/****************************************************************************
 *                                                                          *
 *                            GNATMEM COMPONENTS                            *
 *                                                                          *
 *                                 G M E M                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *         Copyright (C) 2000-2009, Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This unit reads the allocation tracking log produced by augmented
    __gnat_malloc and __gnat_free procedures (see file memtrack.adb) and
    provides GNATMEM tool with gdb-compliant output. The output is
    processed by GNATMEM to detect dynamic memory allocation errors.

    See GNATMEM section in GNAT User's Guide for more information.

    NOTE: This capability is currently supported on the following targets:

      DEC Unix
      GNU/Linux x86
      Solaris (sparc and x86) (*)
      Windows 98/95/NT (x86)
      Alpha OpenVMS

    (*) on these targets, the compilation must be done with -funwind-tables to
    be able to build the stack backtrace.

*/

#ifdef VMS
#include <string.h>
#define xstrdup32(S)  strcpy ((__char_ptr32) _malloc32 (strlen (S) + 1), S)
#else
#define xstrdup32(S) S
#endif

#include <stdio.h>

static FILE *gmemfile;

/* tb_len is the number of call level supported by this module */
#define tb_len 200
static void * tracebk [tb_len];
static int cur_tb_len, cur_tb_pos;

#define LOG_EOF   '*'
#define LOG_ALLOC 'A'
#define LOG_DEALL 'D'

struct struct_storage_elmt {
  char   Elmt;
  void * Address;
  size_t Size;
  long long Timestamp;
};

static void
__gnat_convert_addresses (void *addrs[], int n_addrs, void *buf, int *len);
/* Place in BUF a string representing the symbolic translation of N_ADDRS raw
   addresses provided in ADDRS.  LEN is filled with the result length.

   This is a GNAT specific interface to the libaddr2line convert_addresses
   routine.  The latter examines debug info from a provided executable file
   name to perform the translation into symbolic form of an input sequence of
   raw binary addresses.  It attempts to open the file from the provided name
   "as is", so an absolute path must be provided to ensure the file is
   always found.  We compute this name once, at initialization time.  */

static const char * exename = 0;

extern void convert_addresses (const char * , void *[], int, void *, int *);
extern char  *__gnat_locate_exec_on_path (char *);
/* ??? Both of these extern functions are prototyped in adaint.h, which
   also refers to "time_t" hence needs complex extra header inclusions to
   be satisfied on every target.  */

static void
__gnat_convert_addresses (void *addrs[], int n_addrs, void *buf, int *len)
{
  if (exename != 0)
    convert_addresses (exename, addrs, n_addrs, buf, len);
  else
    *len = 0;
}

/* reads backtrace information from gmemfile placing them in tracebk
   array. cur_tb_len is the size of this array
*/

static void
gmem_read_backtrace (void)
{
  fread (&cur_tb_len, sizeof (int), 1, gmemfile);
  fread (tracebk, sizeof (void *), cur_tb_len, gmemfile);
  cur_tb_pos = 0;
}

/* initialize gmem feature from the dumpname file. It returns t0 timestamp
   if the dumpname has been generated by GMEM (instrumented malloc/free)
   and 0 if not.
*/

long long __gnat_gmem_initialize (char *dumpname)
{
  char header [10];
  long long t0;

  gmemfile = fopen (dumpname, "rb");
  fread (header, 10, 1, gmemfile);

  /* check for GMEM magic-tag */
  if (memcmp (header, "GMEM DUMP\n", 10))
    {
      fclose (gmemfile);
      return 0;
    }

  fread (&t0, sizeof (long long), 1, gmemfile);

  return t0;
}

/* initialize addr2line library */

void __gnat_gmem_a2l_initialize (char *exearg)
{
  /* Resolve the executable filename to use in later invocations of
     the libaddr2line symbolization service. Ensure that on VMS
     exename is allocated in 32 bit memory for compatibility
     with libaddr2line. */
  exename = xstrdup32 (__gnat_locate_exec_on_path (exearg));
}

/* Read next allocation of deallocation information from the GMEM file and
   write an alloc/free information in buf to be processed by gnatmem */

void
__gnat_gmem_read_next (struct struct_storage_elmt *buf)
{
  void *addr;
  size_t size;
  int j;

  j = fgetc (gmemfile);
  if (j == EOF)
    {
      fclose (gmemfile);
      buf->Elmt = LOG_EOF;
    }
  else
    {
      switch (j)
        {
          case 'A' :
            buf->Elmt = LOG_ALLOC;
            fread (&(buf->Address), sizeof (void *), 1, gmemfile);
            fread (&(buf->Size), sizeof (size_t), 1, gmemfile);
            fread (&(buf->Timestamp), sizeof (long long), 1, gmemfile);
            break;
          case 'D' :
            buf->Elmt = LOG_DEALL;
            fread (&(buf->Address), sizeof (void *), 1, gmemfile);
            fread (&(buf->Timestamp), sizeof (long long), 1, gmemfile);
            break;
          default:
            puts ("GNATMEM dump file corrupt");
            __gnat_os_exit (1);
        }

      gmem_read_backtrace ();
    }
}

/* Read the next frame from the current traceback, and move the cursor to the
   next frame */

void __gnat_gmem_read_next_frame (void** addr)
{
  if (cur_tb_pos >= cur_tb_len) {
    *addr = NULL;
  } else {
    *addr = (void*)*(tracebk + cur_tb_pos);
    ++cur_tb_pos;
  }
}

/* Converts addr into a symbolic traceback, and stores the result in buf
   with a format suitable for gnatmem */

void __gnat_gmem_symbolic (void * addr, char* buf, int* length)
{
  void * addresses [] = { addr };

  __gnat_convert_addresses (addresses, 1, buf, length);
}
