/* Stripped down support to run global constructors and destructors on
   embedded PowerPC systems.

   Copyright (C) 1995, 2000 Free Software Foundation, Inc.
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

#include "tm.h"
#include "defaults.h"
#include <stddef.h>
#include "frame.h"
#include "gbl-ctors.h"

/* FIXME: This file should share code with all the other
   constructor/destructor implementations in crtstuff.c and libgcc2.c.  */

/* Declare the set of symbols use as begin and end markers for the lists
   of global object constructors and global object destructors.  */

extern func_ptr __CTOR_LIST__[] asm ("__CTOR_LIST__");
extern func_ptr __CTOR_END__ [] asm ("__CTOR_END__");
extern func_ptr __DTOR_LIST__[] asm ("__DTOR_LIST__");
extern func_ptr __DTOR_END__ [] asm ("__DTOR_END__");
extern unsigned char __EH_FRAME_BEGIN__[] asm ("__EH_FRAME_BEGIN__");

extern void __do_global_ctors (void);

extern void __init (), __fini ();

void (*__atexit)(func_ptr);

/* Call all global constructors */
void
__do_global_ctors (void)
{
  func_ptr *p = __CTOR_END__ - 1;

#ifdef EH_FRAME_SECTION
  {
    static struct object object;
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
  }
#endif

  /* Call the constructors collected in the .ctors section.  */
  for ( ; p >= __CTOR_LIST__; p--)
    if (*p)
      (*p)();
  
  if (__atexit)
    __atexit (__do_global_dtors);

  /* Call the initialization function in the .init section.  */
  __init ();
}

/* Call all global destructors */
void
__do_global_dtors (void)
{
  static func_ptr *p = __DTOR_LIST__ + 1;
  static int completed = 0;

  if (completed)
    return;

  /* Call the termination function in the .fini section.  */
  __fini ();

  while (p < __DTOR_END__)
    {
      p++;
      (*(p-1)) ();
    }

#ifdef EH_FRAME_SECTION_ASM_OP
  if (__deregister_frame_info)
    __deregister_frame_info (__EH_FRAME_BEGIN__);
#endif
  completed = 1;
}
