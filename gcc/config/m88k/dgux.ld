/* m88kdgux.ld - COFF linker directives for G++ on an AViiON

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
   Boston, MA 02111-1307, USA.
   
   On The AViiON we start the output .text section somewhere after the
   first 64kb (0x10000) of logical address space so that the first
   64kb can be mapped out, thus catching references through null
   pointers.  We actually start at 0x10200 (for efficiency).  Ideally,
   we want the page offset of a given word of the .text (output)
   section to be the same as its page offset in the actual (output)
   linked core file so that paging of the .text section is efficient.
   In order to do this we allow for up to 0x200 bytes of header stuff
   in the output (linked) object file.
   
   For .data, the OCS says that regions with different "protections"
   (i.e. read/write, read-only) should not share any 4 megabyte chunk
   of the logical address space, so we start the .data segment at the
   first (lowest) 4 MB boundary past the end of the .text segment.
   
   For some reason, you can't start right at the 4 MB boundary.  You
   have to start at some distance past that.  The distance must be
   equal to the distance from the start of the last 64 KB segment in
   the (output) .text segment to the actual end of the (output) .text
   segment.  */

SECTIONS {
  .text 0x10200 BLOCK(0x200) :
    { *(.init) *(.initp) *(.finip) *(.text) *(.tdesc) }

  GROUP BIND (((((ADDR(.text) + SIZEOF(.text) - 1) / 0x400000) + 1) * 0x400000) + ((ADDR(.text) + SIZEOF (.text)) % 0x10000)) :
    { .data : { *(.data) *(.ctors) *(.dtors) }
      .bss  : {} }
}
