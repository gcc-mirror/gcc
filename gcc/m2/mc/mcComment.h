/* mcComment interface to comment module.

Copyright (C) 2018-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef mcCommentH
#define mcCommentH

#include <stdbool.h>

/* addText the text cs is appended to the current comment.  */

extern void mcComment_addText (void *cd, char *cs);


/* initComment the start of a new comment has been seen by the lexical analyser.
   A new comment block is created and all addText contents are placed
   in this block.  onlySpaces indicates whether we have only seen
   spaces on this line.  The new comment descriptor is returned.
   If onlySpaces is TRUE then an inbody comment is created.
   If onlySpaces is FALSE then an after statement comment is created.  */

extern void *mcComment_initComment (bool onlySpaces);


#endif
