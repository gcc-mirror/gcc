/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               S O C K E T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 2003-2005 Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This file provides a portable binding to the sockets API                */

#include "gsocket.h"
/* Include all the necessary system-specific headers and define the
   necessary macros (shared with gen-soccon). */

#include "raise.h"
/* Required for __gnat_malloc() */

extern void __gnat_disable_sigpipe (int fd);
extern void __gnat_free_socket_set (fd_set *);
extern void __gnat_last_socket_in_set (fd_set *, int *);
extern void __gnat_get_socket_from_set (fd_set *, int *, int *);
extern void __gnat_insert_socket_in_set (fd_set *, int);
extern int __gnat_is_socket_in_set (fd_set *, int);
extern fd_set *__gnat_new_socket_set (fd_set *);
extern void __gnat_remove_socket_from_set (fd_set *, int);

/* Disable the sending of SIGPIPE for writes on a broken stream */
void
__gnat_disable_sigpipe (int fd)
{
#ifdef SO_NOSIGPIPE
  int val = 1;
  (void) setsockopt (fd, SOL_SOCKET, SO_NOSIGPIPE, &val, sizeof val);
#endif
}

/* Free socket set. */

void
__gnat_free_socket_set (fd_set *set)
{
  __gnat_free (set);
}

/* Find the largest socket in the socket set SET. This is needed for
   `select'.  LAST is the maximum value for the largest socket. This hint is
   used to avoid scanning very large socket sets.  On return, LAST is the
   actual largest socket in the socket set. */

void
__gnat_last_socket_in_set (fd_set *set, int *last)
{
  int s;
  int l;
  l = -1;

#ifdef WINNT
  /* More efficient method for NT. */
  for (s = 0; s < set->fd_count; s++)
    if ((int) set->fd_array[s] > l)
      l = set->fd_array[s];

#else

  for (s = *last; s != -1; s--)
    if (FD_ISSET (s, set))
      {
	l = s;
	break;
      }
#endif

  *last = l;
}

/* Get last socket and remove it from the socket set SET.  LAST is the
   maximum value of the largest socket.  This hint is used to avoid scanning
   very large socket sets.  On return, LAST is set to the actual largest
   socket in the socket set. */

void
__gnat_get_socket_from_set (fd_set *set, int *last, int *socket)
{
  *socket = *last;
  FD_CLR (*socket, set);
  __gnat_last_socket_in_set (set, last);
}

/* Insert SOCKET in the socket set SET. */

void
__gnat_insert_socket_in_set (fd_set *set, int socket)
{
  FD_SET (socket, set);
}

/* Check whether a given SOCKET is in the socket set SET. */

int
__gnat_is_socket_in_set (fd_set *set, int socket)
{
  return FD_ISSET (socket, set);
}

/* Allocate a new socket set and set it as empty.  */

fd_set *
__gnat_new_socket_set (fd_set *set)
{
  fd_set *new;

  new = (fd_set *) __gnat_malloc (sizeof (fd_set));

  if (set)
    memcpy (new, set, sizeof (fd_set));
  else
    FD_ZERO (new);

  return new;
}

/* Remove SOCKET from the socket set SET. */

void
__gnat_remove_socket_from_set (fd_set *set, int socket)
{
  FD_CLR (socket, set);
}
