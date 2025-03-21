(* M2RTS.mod implements access to the exception handlers.

Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2RTS ;


FROM libc IMPORT abort, exit, write, getenv, printf, strlen ;
(* FROM Builtins IMPORT strncmp, strcmp ;  not available during bootstrap.  *)
FROM NumberIO IMPORT CardToStr ;
FROM StrLib IMPORT StrCopy, StrLen, StrEqual ;
FROM SYSTEM IMPORT ADR ;
FROM ASCII IMPORT nl, nul ;
FROM Storage IMPORT ALLOCATE ;

IMPORT RTExceptions ;
IMPORT M2EXCEPTION ;
IMPORT M2Dependent ;

CONST
   stderrFd = 2 ;

TYPE
   PtrToChar = POINTER TO CHAR ;

VAR
   ExitValue          : INTEGER ;
   isTerminating,
   isHalting,
   Initialized,
   CallExit           : BOOLEAN ;


(*
   ConstructModules - resolve dependencies and then call each
                      module constructor in turn.
*)

PROCEDURE ConstructModules (applicationmodule, libname: ADDRESS;
                            overrideliborder: ADDRESS;
                            argc: INTEGER; argv, envp: ADDRESS) ;
BEGIN
   M2Dependent.ConstructModules (applicationmodule, libname,
                                 overrideliborder,
                                 argc, argv, envp)
END ConstructModules ;


(*
   DeconstructModules - resolve dependencies and then call each
                        module constructor in turn.
*)

PROCEDURE DeconstructModules (applicationmodule, libname: ADDRESS;
                              argc: INTEGER; argv, envp: ADDRESS) ;
BEGIN
   M2Dependent.DeconstructModules (applicationmodule, libname,
                                   argc, argv, envp)
END DeconstructModules ;


(*
   RegisterModule - adds module name to the list of outstanding
                    modules which need to have their dependencies
                    explored to determine initialization order.
*)

PROCEDURE RegisterModule (name, libname: ADDRESS;
                          init, fini:  ArgCVEnvP;
                          dependencies: PROC) ;
BEGIN
   M2Dependent.RegisterModule (name, libname, init, fini, dependencies)
END RegisterModule ;


(*
   RequestDependant - used to specify that modulename is dependant upon
                      module dependantmodule.
*)

PROCEDURE RequestDependant (modulename, libname,
                            dependantmodule, dependantlibname: ADDRESS) ;
BEGIN
   M2Dependent.RequestDependant (modulename, libname,
                                 dependantmodule, dependantlibname)
END RequestDependant ;


(*
   ExecuteTerminationProcedures - calls each installed termination procedure
                                  in reverse order.
*)

PROCEDURE ExecuteTerminationProcedures ;
BEGIN
   M2Dependent.ExecuteTerminationProcedures
END ExecuteTerminationProcedures ;


(*
   ExecuteInitialProcedures - executes the initial procedures installed by
                              InstallInitialProcedure.
*)

PROCEDURE ExecuteInitialProcedures ;
BEGIN
   M2Dependent.ExecuteInitialProcedures
END ExecuteInitialProcedures ;


(*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure
                                 ExecuteTerminationProcedures
                                 is invoked.  It returns TRUE if the
                                 procedure is installed.
*)

PROCEDURE InstallTerminationProcedure (p: PROC) : BOOLEAN ;
BEGIN
   RETURN M2Dependent.InstallTerminationProcedure (p)
END InstallTerminationProcedure ;


(*
   InstallInitialProcedure - installs a procedure to be executed just
                             before the BEGIN code section of the
                             main program module.
*)

PROCEDURE InstallInitialProcedure (p: PROC) : BOOLEAN ;
BEGIN
   RETURN M2Dependent.InstallInitialProcedure (p)
END InstallInitialProcedure ;


(*
   HALT - terminate the current program.  The procedure
          ExecuteTerminationProcedures
          is called before the program is stopped.  The parameter
          exitcode is optional.  If the parameter is not supplied
          HALT will call libc 'abort', otherwise it will exit with
          the code supplied.  Supplying a parameter to HALT has the
          same effect as calling ExitOnHalt with the same code and
          then calling HALT with no parameter.
*)

PROCEDURE HALT ([exitcode: INTEGER = -1]) <* noreturn *> ;
BEGIN
   IF exitcode#-1
   THEN
      CallExit := TRUE ;
      ExitValue := exitcode
   END ;
   IF isHalting
   THEN
      (* double HALT found *)
      exit(-1)
   ELSE
      isHalting := TRUE ;
      ExecuteTerminationProcedures ;
   END ;
   IF CallExit
   THEN
      exit(ExitValue)
   ELSE
      abort
   END
END HALT ;


(*
   Terminate - provides compatibility for pim.  It call exit with
               the exitcode provided in a prior call to ExitOnHalt
               (or zero if ExitOnHalt was never called).  It does
               not call ExecuteTerminationProcedures.
*)

PROCEDURE Terminate <* noreturn *> ;
BEGIN
   exit (ExitValue)
END Terminate ;


(*
   ErrorString - writes a string to stderr.
*)

PROCEDURE ErrorString (a: ARRAY OF CHAR) ;
VAR
   n: INTEGER ;
BEGIN
   n := write (stderrFd, ADR (a), StrLen (a))
END ErrorString ;


(*
   ErrorStringC - writes a string to stderr.
*)

PROCEDURE ErrorStringC (str: ADDRESS) ;
VAR
   len: INTEGER ;
BEGIN
   len := write (stderrFd, str, strlen (str))
END ErrorStringC ;


(*
   ErrorMessage - emits an error message to stderr and then calls exit (1).
*)

PROCEDURE ErrorMessage (message: ARRAY OF CHAR;
                        filename: ARRAY OF CHAR;
                        line: CARDINAL;
                        function: ARRAY OF CHAR) <* noreturn *> ;
VAR
   buffer: ARRAY [0..10] OF CHAR ;
BEGIN
   ErrorString (filename) ; ErrorString(':') ;
   CardToStr (line, 0, buffer) ;
   ErrorString (buffer) ; ErrorString(':') ;
   IF NOT StrEqual (function, '')
   THEN
      ErrorString ('in ') ;
      ErrorString (function) ;
      ErrorString (' has caused ') ;
   END ;
   ErrorString (message) ;
   buffer[0] := nl ; buffer[1] := nul ;
   ErrorString (buffer) ;
   exit (1)
END ErrorMessage ;


(*
   ErrorMessageC - emits an error message to stderr and then calls exit (1).
*)

PROCEDURE ErrorMessageC (message, filename: ADDRESS;
                         line: CARDINAL;
                         function: ADDRESS) <* noreturn *> ;
VAR
   buffer: ARRAY [0..10] OF CHAR ;
BEGIN
   ErrorStringC (filename) ; ErrorString (':') ;
   CardToStr (line, 0, buffer) ;
   ErrorString (buffer) ; ErrorString(':') ;
   IF strlen (function) > 0
   THEN
      ErrorString ('in ') ;
      ErrorStringC (function) ;
      ErrorString (' has caused ') ;
   END ;
   ErrorStringC (message) ;
   buffer[0] := nl ; buffer[1] := nul ;
   ErrorString (buffer) ;
   exit (1)
END ErrorMessageC ;


(*
   HaltC - provides a more user friendly version of HALT, which takes
           four parameters to aid debugging.  It writes an error message
           to stderr and calls exit (1).
*)

PROCEDURE HaltC (description, filename, function: ADDRESS; line: CARDINAL) ;
BEGIN
   ErrorMessageC (description, filename, line, function)
END HaltC ;


(*
   Halt - provides a more user friendly version of HALT, which takes
          four parameters to aid debugging.  It writes an error message
          to stderr and calls exit (1).
*)

PROCEDURE Halt (description, filename, function: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   ErrorMessage (description, filename, line, function)
END Halt ;


(*
   IsTerminating - Returns true if any coroutine has started program termination
                   and false otherwise.
*)

PROCEDURE IsTerminating () : BOOLEAN ;
BEGIN
   RETURN isTerminating
END IsTerminating ;


(*
   HasHalted - Returns true if a call to HALT has been made and false
               otherwise.
*)

PROCEDURE HasHalted () : BOOLEAN ;
BEGIN
   RETURN isHalting
END HasHalted ;


(*
   ErrorCharStar -
*)

PROCEDURE ErrorCharStar (a: ADDRESS) ;
VAR
   p: POINTER TO CHAR ;
   n: INTEGER ;
BEGIN
   p := a ;
   n := 0 ;
   WHILE (p#NIL) AND (p^#nul) DO
      INC(n) ;
      INC(p)
   END ;
   IF n>0
   THEN
      n := write(2, a, n)
   END
END ErrorCharStar ;


(*
   ErrorMessageColumn - emits an error message to the stderr
*)

PROCEDURE ErrorMessageColumn (filename, scope, message: ADDRESS;
                              line, column: CARDINAL) ;
VAR
   LineNo: ARRAY [0..10] OF CHAR ;
BEGIN
   ErrorCharStar(filename) ; ErrorString(':') ;
   CardToStr(line, 0, LineNo) ;
   ErrorString(LineNo) ; ErrorString(':') ;
   CardToStr(column, 0, LineNo) ;
   ErrorString(LineNo) ; ErrorString(':') ;
   ErrorCharStar(scope) ; ErrorString(':') ;
   ErrorCharStar(message) ;
   LineNo[0] := nl ; LineNo[1] := nul ;
   ErrorString(LineNo) ;
   exit(1)
END ErrorMessageColumn ;


(*
   ExitOnHalt - if HALT is executed then call exit with the exit code, e.
*)

PROCEDURE ExitOnHalt (e: INTEGER) ;
BEGIN
   ExitValue := e ;
   CallExit := TRUE
END ExitOnHalt ;


(*
   Length - returns the length of a string, a. This is called whenever
            the user calls LENGTH and the parameter cannot be calculated
            at compile time.
*)

PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   l, h: CARDINAL ;
BEGIN
   l := 0 ;
   h := HIGH(a) ;
   WHILE (l<=h) AND (a[l]#nul) DO
      INC(l)
   END ;
   RETURN( l )
END Length ;


(*
   The following are the runtime exception handler routines.
*)

PROCEDURE AssignmentException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END AssignmentException ;


PROCEDURE ReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END ReturnException ;


PROCEDURE IncException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END IncException ;


PROCEDURE DecException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END DecException ;


PROCEDURE InclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END InclException ;


PROCEDURE ExclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END ExclException ;


PROCEDURE ShiftException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END ShiftException ;


PROCEDURE RotateException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END RotateException ;


PROCEDURE StaticArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise(ORD (M2EXCEPTION.indexException),
                      filename, line, column, scope, message)
END StaticArraySubscriptException ;


PROCEDURE DynamicArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.indexException),
                       filename, line, column, scope, message)
END DynamicArraySubscriptException ;


PROCEDURE ForLoopBeginException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END ForLoopBeginException ;


PROCEDURE ForLoopToException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END ForLoopToException ;


PROCEDURE ForLoopEndException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END ForLoopEndException ;


PROCEDURE PointerNilException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.invalidLocation),
                       filename, line, column, scope, message)
END PointerNilException ;


PROCEDURE NoReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.functionException),
                       filename, line, column, scope, message)
END NoReturnException ;


PROCEDURE CaseException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.caseSelectException),
                       filename, line, column, scope, message)
END CaseException ;


PROCEDURE WholeNonPosDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.wholeDivException),
                       filename, line, column, scope, message)
END WholeNonPosDivException ;


PROCEDURE WholeNonPosModException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.wholeDivException),
                       filename, line, column, scope, message)
END WholeNonPosModException ;


PROCEDURE WholeZeroDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.wholeDivException),
                       filename, line, column, scope, message)
END WholeZeroDivException ;


PROCEDURE WholeZeroRemException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.wholeDivException),
                       filename, line, column, scope, message)
END WholeZeroRemException ;


PROCEDURE WholeValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.wholeValueException),
                       filename, line, column, scope, message)
END WholeValueException ;


PROCEDURE RealValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.realValueException),
                       filename, line, column, scope, message)
END RealValueException ;


PROCEDURE ParameterException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.rangeException),
                       filename, line, column, scope, message)
END ParameterException ;


PROCEDURE NoException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
BEGIN
   RTExceptions.Raise (ORD (M2EXCEPTION.exException),
                       filename, line, column, scope, message)
END NoException ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   ExitValue := 0 ;
   isHalting := FALSE ;
   CallExit := FALSE ;  (* default by calling abort *)
   isTerminating := FALSE
END Init ;


(*
   CheckInitialized - checks to see if this module has been initialized
                      and if it has not it calls Init.  We need this
                      approach as this module is called by module ctors
                      before we reach main.
*)

PROCEDURE CheckInitialized ;
BEGIN
   IF NOT Initialized
   THEN
      Initialized := TRUE ;
      Init
   END
END CheckInitialized ;


BEGIN
   (* Initialized := FALSE ;  is achieved though setting the bss section to zero.  *)
   CheckInitialized
END M2RTS.
