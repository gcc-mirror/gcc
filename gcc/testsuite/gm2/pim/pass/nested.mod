(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
                 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
   Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE nested ;


PROCEDURE outer (o1, o2, o3: CARDINAL) : CARDINAL ;
   PROCEDURE inner (i1, i2: CARDINAL) : CARDINAL ;
   BEGIN
      RETURN( i1+i2+o3 )
   END inner ;

BEGIN
   RETURN( inner(o1, o2) )
END outer ;

VAR
   g: CARDINAL ;
BEGIN
   g := outer(1, 2, 3)
END nested.
