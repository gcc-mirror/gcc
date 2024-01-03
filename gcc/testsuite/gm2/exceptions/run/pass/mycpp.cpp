/* Copyright (C) 2008-2024 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#include <stdio.h>
#include <stdlib.h>

extern "C" void mycpp_test (void);
extern "C" void m2test_try (void);

void mycpp_test (void)
{
  int r;

  try {
    printf("start of main c++ program\n");
    m2test_try() ;
    printf("ending (should not get here)\n");
    exit(1);
  }
  catch (int i) {
    printf("c++ caught exception correctly\n");
    exit(0);
  }
  printf("c++ should not get here\n");
  exit(1);
}

extern "C" void _M2_mycpp_init (void)
{
  mycpp_test();
}

extern "C" void _M2_mycpp_finish (void)
{
}

extern "C" void _M2_mycpp_ctor (void)
{
}
