/* Implement Input/Output runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser, et al

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

#include "fileio.h"

void
__settextindex( Text_Mode*  the_text,
                signed long the_text_index, 
                char*       file,
                int         line )
{
  if( !the_text )
    CHILLEXCEPTION( file, line, EMPTY, NULL_TEXT );

  if( the_text_index < 0 
      || the_text->access_sub->reclength - 2 < the_text_index )
    CHILLEXCEPTION( file, line, TEXTFAIL, BAD_TEXTINDEX );
  
  the_text->actual_index = the_text_index;
}

