/* m2rts.cc provides a C interface to M2RTS.mod.

Copyright (C) 2019-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This is a minimal wrapper for M2RTS.c which allows mc to be built with
   a nul pathname "m2pim" library and then to link against an installed
   m2pim library.  */

typedef void (*proc_con) (int, char **, char **);
typedef void (*proc_dep) (void);

extern "C" void m2pim_M2RTS_RegisterModule (void *modulename, void *libname,
					    proc_con init, proc_con fini, proc_dep dependencies);

/* Fixup references, the code will not be used though, as it is only used if
   -fscaffold-dynamic is selected (and mc uses -fscaffold-static).  */

extern "C"
void M2RTS_RegisterModule (char *modulename, char *libname,
			   proc_con init, proc_con fini, proc_dep dependencies)
{
  m2pim_M2RTS_RegisterModule (reinterpret_cast <void *> (modulename),
			      reinterpret_cast <void *> (libname),
			      init, fini, dependencies);
}
