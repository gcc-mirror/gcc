(* SRawIO.mod implement the ISO SRawIO specification.

Copyright (C) 2009-2024 Free Software Foundation, Inc.
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
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SRawIO ;

IMPORT StdChans, RawIO ;

(*
   Read - reads storage units from the default input channel, and
          assigns them to successive components of to.  The read
          result is set to the value allRight, wrongFormat, or
          endOfInput.
*)

PROCEDURE Read (VAR to: ARRAY OF SYSTEM.LOC) ;
BEGIN
   RawIO.Read(StdChans.StdInChan(), to)
END Read ;


(*
   Write - writes storage units to the default output channel from
           successive components of from.
*)

PROCEDURE Write (from: ARRAY OF SYSTEM.LOC) ;
BEGIN
   RawIO.Write(StdChans.StdOutChan(), from)   
END Write ;


END SRawIO.
