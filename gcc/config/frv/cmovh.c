/* Move half-word library function.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.
  
   This file is part of GNU CC.
  
   GNU CC is free software ; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation * either version 2, or (at your option)
   any later version.
  
   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY ; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
  
   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

void
__cmovh (short *dest, const short *src, unsigned len)
{
  unsigned i;
  unsigned num = len >> 1;
  char *dest_byte = (char *)dest;
  const char *src_byte = (const char *)src;

  if (dest_byte < src_byte || dest_byte > src_byte+len)
    {
      for (i = 0; i < num; i++)
	dest[i] = src[i];

      if ((len & 1) != 0)
	dest_byte[len-1] = src_byte[len-1];
    }
  else
    {
      while (len-- > 0)
	dest_byte[len] = src_byte[len];
    }
}
