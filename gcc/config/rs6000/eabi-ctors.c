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

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


/*  Declare a pointer to void function type.  */

typedef void (*func_ptr) (void);

/* Declare the set of symbols use as begin and end markers for the lists
   of global object constructors and global object destructors.  */

extern func_ptr __CTOR_LIST__[];
extern func_ptr __CTOR_END__ [];
extern func_ptr __DTOR_LIST__[];
extern func_ptr __DTOR_END__ [];

extern void __do_global_ctors (void);
extern void __do_global_dtors (void);

extern void __init (), __fini ();

/* The Solaris linker seems to incorrectly relocate PC relative relocations
   to a different section (ie, calls to __init, __fini), so avoid it by
   using a function pointer.  */
static void (*init_ptr) (void) = __init;
static void (*fini_ptr) (void) = __fini;

void (*__atexit)(func_ptr);

/* Call all global constructors */
void
__do_global_ctors (void)
{
  func_ptr *ptr   = &__CTOR_END__[0] - 1;
  func_ptr *start = &__CTOR_LIST__[0];

  if (__atexit)
    __atexit (__do_global_dtors);

  /* Call the constructors collected in the .ctors section.  */
  for ( ; ptr >= start; ptr--)
    if (*ptr)
      (*ptr)();

  /* Call the initialization function in the .init section.  */
  (*init_ptr) ();
}

/* Call all global destructors */
void
__do_global_dtors (void)
{
  func_ptr *ptr   = &__DTOR_LIST__[0];
  func_ptr *end   = &__DTOR_END__[0];

  /* Call the termination function in the .fini section.  */
  (*fini_ptr) ();

  /* Call the  destructors collected in the .dtors section.  Run
     the destructors in reverse order.  */
  for ( ; ptr < end; ptr++)
    if (*ptr)
      (*ptr)();
}

