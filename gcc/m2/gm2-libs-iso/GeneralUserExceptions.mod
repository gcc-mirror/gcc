(* GeneralUserExceptions.mod implement the ISO GeneralUserExceptions.

Copyright (C) 2002-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE GeneralUserExceptions ;

FROM EXCEPTIONS IMPORT ExceptionSource, RAISE, AllocateSource, CurrentNumber,
                       IsCurrentSource, IsExceptionalExecution ;

FROM M2RTS IMPORT NoException ;
FROM SYSTEM IMPORT ADR ;


VAR
   general: ExceptionSource ;


(*
   RaiseGeneralException - raises exception using text as the associated
                           message.
*)

PROCEDURE RaiseGeneralException (exception: GeneralExceptions; text: ARRAY OF CHAR) ;
BEGIN
   RAISE (general, ORD (exception), text)
END RaiseGeneralException ;


(*
   IsGeneralException - returns TRUE if the current coroutine is in the
                        exceptional execution state because of the raising
                        of an exception from GeneralExceptions; otherwise
                        returns FALSE.
*)

PROCEDURE IsGeneralException () : BOOLEAN ;
BEGIN
   RETURN IsExceptionalExecution () AND IsCurrentSource (general)
END IsGeneralException ;


(*
   GeneralException - if the current coroutine is in the exceptional
                      execution state because of the raising of an
                      exception from GeneralExceptions, returns the
                      corresponding enumeration value, and otherwise
                      raises an exception.
*)

PROCEDURE GeneralException () : GeneralExceptions;
BEGIN
   IF IsGeneralException ()
   THEN
      RETURN VAL (GeneralExceptions, CurrentNumber (general))
   ELSE
      NoException (ADR (__FILE__), __LINE__,
                   __COLUMN__, ADR (__FUNCTION__),
      ADR ("not in the exceptional execution state"))
   END
END GeneralException ;


BEGIN
   AllocateSource (general)
END GeneralUserExceptions.
