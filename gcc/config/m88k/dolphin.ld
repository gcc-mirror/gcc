/* COFF linker directives for the Dolphin Triton88 for GNU compiler.
   Copyright (C) 1993 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


This file does the following:

    - Sets VIRTUAL addr of .text to 0x400200 
      Sets FILE addr of .text to 0x200 (BLOCK directive)
    - Depending on size of .text section rounds up to next
      4 MG boundary, adds (size of .text and vaddr of .text) mod 64K
      This is to handle sections larger than 4 MG.   */

SECTIONS {
  .text 0x400200 BLOCK (0x200):
    { *(.init) *(.text) *(.rodata) *(.tdesc) *(.fini)}

  GROUP BIND( (((SIZEOF(.text)+ADDR(.text)) / 0x400000 * 0x400000) + 0x400000) +
    ((SIZEOF(.text) + ADDR(.text)) % 0x10000) ) :
    {
      .data : { }
      .bss  : { }
    }
}
