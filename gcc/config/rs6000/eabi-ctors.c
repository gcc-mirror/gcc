/* Stripped down support to run global constructors and destructors on
   embedded PowerPC systems.

   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by Michael Meissner  (meissner@cygnus.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/*  Declare a pointer to void function type.  */

typedef void (*func_ptr) (void);

/* Declare the set of symbols use as begin and end markers for the lists
   of global object constructors and global object destructors.  */

extern func_ptr __CTOR_LIST__[];
extern func_ptr __CTOR_END__ [];
extern func_ptr __DTOR_LIST__[];
extern func_ptr __DTOR_END__ [];

/* Call all global constructors */
void
__do_global_ctors (void)
{
  func_ptr *ptr = &__CTOR_LIST__[0];
  func_ptr *end = &__CTOR_END__[0];

  for ( ; ptr != end; ptr++)
    (*ptr)();
}

/* Call all global destructors */
void
__do_global_dtors (void)
{
  func_ptr *ptr = &__DTOR_LIST__[0];
  func_ptr *end = &__DTOR_END__[0];

  for ( ; ptr != end; ptr++)
    (*ptr)();
}

