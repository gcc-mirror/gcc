(* M2EXCEPTION.mod implement M2Exception and IsM2Exception.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2EXCEPTION ;

FROM SYSTEM IMPORT ADR ;
FROM RTExceptions IMPORT EHBlock, GetExceptionBlock, GetNumber, Raise,
                         SetExceptionBlock, InitExceptionBlock ;


(* If the program or coroutine is in the exception state then return the enumeration
   value representing the exception cause.  If it is not in the exception state then
   raises and exception (exException).  *)

PROCEDURE M2Exception () : M2Exceptions;
VAR
   e: EHBlock ;
   n: CARDINAL ;
BEGIN
   e := GetExceptionBlock () ;
   n := GetNumber (e) ;
   IF n = MAX (CARDINAL)
   THEN
      Raise (ORD (exException), ADR (__FILE__), __LINE__, __COLUMN__, ADR (__FUNCTION__),
             ADR ('current coroutine is not in the exceptional execution state'))
   ELSE
      RETURN VAL (M2Exceptions, n)
   END
END M2Exception ;


(* Returns TRUE if the program or coroutine is in the exception state.
   Returns FALSE if the program or coroutine is not in the exception state.  *)

PROCEDURE IsM2Exception () : BOOLEAN;
VAR
   e: EHBlock ;
BEGIN
   e := GetExceptionBlock () ;
   RETURN GetNumber (e) # MAX (CARDINAL)
END IsM2Exception ;


BEGIN
   SetExceptionBlock (InitExceptionBlock ())
END M2EXCEPTION.
