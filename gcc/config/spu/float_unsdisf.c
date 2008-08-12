/* Copyright (C) 2008 Free Software Foundation, Inc.
  
   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option)
   any later version.
  
   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.
  
   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License.  The exception does not
   however invalidate any other reasons why the executable file might be covered
   by the GNU General Public License. */

float __floatundisf (unsigned long long x)
{
  /* The SPU back-end now generates inline code for this conversion.
     This file is solely used to provide the __floatundisf function
     for objects generated with prior versions of GCC.  */
  return x;
}
