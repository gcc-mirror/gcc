(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

MODULE nestediso ;


TYPE
   StringType = ARRAY [0..79] OF CHAR ;
   DateType   = RECORD
                   y, m, d: CARDINAL ;
                END ;
   PersonType = RECORD
                   name: StringType ;
                   birth: DateType ;
                END ;

VAR
   person: PersonType ;
   date  : DateType ;
   a, b, c: CARDINAL ;
BEGIN
   date := DateType{1623, 6, 19} ;
   a := 1623 ;
   b := 6 ;
   c := 19 ;
   date := DateType{a, b, c} ;
   person := PersonType{StringType{"" BY 80}, DateType{0, 1, 2}} ;
   person := PersonType{StringType{"" BY 80}, {0, 1, 2}} ;
   person := PersonType{"", {0, 1, 2}} ;
   person := PersonType{StringType{""}, {0, 1, 2}} ;
   person := PersonType{"Blaise Pascal", date}
END nestediso.
