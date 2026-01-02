(* M2Diagnotic provides memory and time diagnosics to the user.

Copyright (C) 2024-2026 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Diagnostic ;  (*!m2iso+gm2*)

FROM ASCII IMPORT nl ;
FROM Selective IMPORT Timeval, GetTimeOfDay, InitTime, GetTime, SetTime ;
FROM StringConvert IMPORT LongCardinalToString, ctos ;
FROM Storage IMPORT ALLOCATE ;
FROM Indexing IMPORT Index ;

FROM DynamicStrings IMPORT InitString, ConCat, KillString, ConCatChar,
                           Equal, Mark, Length, char, RIndex ;

IMPORT DynamicStrings, Indexing ;


CONST
   EnableDiagnostics = TRUE ;    (* If set to FALSE then it will ensure
                                    this module has minimal impact upon
                                    the rest of the application.   *)
   DefaultTimeEnableValue = FALSE ;  (* Should the diagnostics be
                                        enabled by default.  *)
   DefaultMemEnableValue = FALSE ;  (* Should the diagnostics be
                                       enabled by default.  *)

   MaxParam = 4 ;   (* The maximum number of parameters for a mem
                       diag.  *)
   MICROSEC = 1000 * 1000 ;   (* The number of microseconds in a second.  *)

TYPE
   Diagnostic = POINTER TO RECORD
                              name, format: String ;
                              enable      : BOOLEAN ;
                              next        : Diagnostic ;
                              CASE type: DiagType OF

                              timediag: tdiag: timeDiag |
                              memdiag : mdiag: memDiag

                              END
                            END ;

   DiagType = (timediag, memdiag) ;

   timeDiag = RECORD
                 count: CARDINAL ;
                 total,
                 enter,
                 exit : Timeval ;
              END ;

   memDiag = RECORD
                param: ARRAY [1..MaxParam] OF LONGCARD ;
             END ;

VAR
   Output            : String ;
   TotalHeap         : LONGCARD ;
   Head              : Diagnostic ;
   EnableHierarchical,
   DefaultTimeEnable,
   DefaultMemEnable  : BOOLEAN ;
   StartTime,
   TotalTime         : Timeval ;


(*
   Assert - halt if b is false.
*)

PROCEDURE Assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      HALT
   END
END Assert ;


(*
   Error - generate a error simple message with indicating the
           format specifier ch is incorrect.
*)

PROCEDURE Error (msg: ARRAY OF CHAR; ch: CHAR) ;
BEGIN
   HALT
END Error ;


(*
   InitTimeDiagnostic - create and return a time diagnostic.
                        The format string can be free form and may
                        contain {1T}, {1C} or {1P}.
                        {1T} will contain the time and
                        {1C} the count of the number of times the
                        code enters the time diagnostic code region.
                        {1P} generates the time as a percentage.
                        {0T} is the total time for the application.
                        {{ is rendered as a single {.
*)

PROCEDURE InitTimeDiagnostic (name, format: ARRAY OF CHAR) : Diagnostic ;
VAR
   d: Diagnostic ;
BEGIN
   IF EnableDiagnostics
   THEN
      NEW (d) ;
      d^.name := InitString (name) ;
      d^.format := InitString (format) ;
      WITH d^ DO
         enable := DefaultTimeEnable ;
         next := Head ;
         type := timediag ;
         CASE type OF

         timediag: tdiag.count := 0 ;
                   tdiag.total := InitTime (0, 0) ;
                   tdiag.enter := InitTime (0, 0) ;
                   tdiag.exit  := InitTime (0, 0)

         ELSE
            HALT
         END
      END ;
      Head := d ;
      RETURN d
   ELSE
      RETURN NIL
   END
END InitTimeDiagnostic ;


(*
   EnterDiagnostic - attribute all execution time from now to TimeDiag.
*)

PROCEDURE EnterDiagnostic (TimeDiag: Diagnostic) ;
BEGIN
   IF EnableDiagnostics AND (TimeDiag # NIL)
   THEN
      Assert (TimeDiag^.type = timediag) ;
      Assert (GetTimeOfDay (TimeDiag^.tdiag.enter) = 0) ;
      INC (TimeDiag^.tdiag.count)
   END
END EnterDiagnostic ;


(*
   ExitDiagnostic - stop attributing execution time to TimeDiag.
*)

PROCEDURE ExitDiagnostic (TimeDiag: Diagnostic) ;
BEGIN
   IF EnableDiagnostics AND (TimeDiag # NIL)
   THEN
      Assert (TimeDiag^.tdiag.enter # NIL) ;
      Assert (TimeDiag^.tdiag.exit # NIL) ;
      Assert (TimeDiag^.tdiag.total # NIL) ;
      Assert (TimeDiag^.type = timediag) ;
      Assert (GetTimeOfDay (TimeDiag^.tdiag.exit) = 0) ;
      Accumulate (TimeDiag^.tdiag.total, TimeDiag^.tdiag.enter, TimeDiag^.tdiag.exit)
   END
END ExitDiagnostic ;


(*
   Accumulate - total := total + exit - enter
*)

PROCEDURE Accumulate (total, enter, exit: Timeval) ;
BEGIN
   IncTime (total, exit) ;
   DecTime (total, enter)
END Accumulate ;


(*
   IncTime - left := left + right.
*)

PROCEDURE IncTime (left, right: Timeval) ;
VAR
   lsec, lusec,
   rsec, rusec: CARDINAL ;
BEGIN
   GetTime (left, lsec, lusec) ;
   GetTime (right, rsec, rusec) ;
   IF lusec + rusec < MICROSEC
   THEN
      (* No carry  *)
      INC (lusec, rusec) ;
      INC (lsec, rsec)
   ELSE
      INC (lusec, rusec) ;
      DEC (lusec, MICROSEC) ;
      INC (lsec, rsec + 1)
   END ;
   SetTime (left, lsec, lusec)
END IncTime ;


(*
   DecTime - left := left - right.
*)

PROCEDURE DecTime (left, right: Timeval) ;
VAR
   lsec, lusec,
   rsec, rusec: CARDINAL ;
BEGIN
   GetTime (left, lsec, lusec) ;
   GetTime (right, rsec, rusec) ;
   IF lusec >= rusec
   THEN
      (* No borrow.  *)
      DEC (lusec, rusec) ;
      IF lsec >= rsec
      THEN
         DEC (lsec, rsec)
      ELSE
         lsec := 0
      END
   ELSE
      IF lsec > 0
      THEN
         INC (lusec, MICROSEC) ;
         DEC (lusec, rusec) ;
         DEC (lsec) ;
         IF lsec >= rsec
         THEN
            DEC (lsec, rsec)
         ELSE
            lsec := 0
         END
      ELSE
         lsec := 0 ;
         lusec := 0
      END
   END ;
   SetTime (left, lsec, lusec)
END DecTime ;


(*
   InitMemDiagnostic - create and return a memory diagnostic.
                       The format string can be free form and may
                       contain {1M} {1d} {1x} {1P}.
                       {1M} is replaced by the value of the first parameter
                       with memory size units.
                       {1d} unsigned decimal.  {1x} unsigned hexadecimal.
                       {0M} is the global allocation (Storage.mod:ALLOCATE).
                       {1P} is the percentage of param 1 relative
                       to global memory.
*)

PROCEDURE InitMemDiagnostic (name, format: ARRAY OF CHAR) : Diagnostic ;
VAR
   i: CARDINAL ;
   d: Diagnostic ;
BEGIN
   IF EnableDiagnostics
   THEN
      NEW (d) ;
      d^.name := InitString (name) ;
      d^.format := InitString (format) ;
      WITH d^ DO
         enable := DefaultMemEnable ;
         next := Head ;
         type := memdiag ;
         CASE type OF

         memdiag: FOR i := 1 TO MaxParam DO
                     mdiag.param[i] := 0
                  END

         ELSE
            HALT
         END
      END ;
      Head := d ;
      RETURN d
   ELSE
      RETURN NIL
   END
END InitMemDiagnostic ;


(*
   CheckParam -
*)

PROCEDURE CheckParam (paramno: CARDINAL) ;
BEGIN
   IF (paramno < 1) OR (paramno > MaxParam)
   THEN
      HALT
   END
END CheckParam ;


(*
   MemIncr - allow the appropriate parameter to be incremented.
             All parameters are initially set to zero and are stored
             as LONGCARD.
*)

PROCEDURE MemIncr (MemDiag: Diagnostic; paramno: CARDINAL; incr: CARDINAL) ;
BEGIN
   IF EnableDiagnostics AND (MemDiag # NIL)
   THEN
      CheckParam (paramno) ;
      CASE MemDiag^.type OF

      memdiag:  INC (MemDiag^.mdiag.param[paramno], VAL (LONGCARD, incr))

      ELSE
         HALT
      END
   END
END MemIncr ;


(*
   MemDecr - allow the appropriate parameter to be decremented.
             All parameters are initially set to zero and are stored
             as LONGCARD.
*)

PROCEDURE MemDecr (MemDiag: Diagnostic; paramno: CARDINAL; decr: CARDINAL) ;
BEGIN
   IF EnableDiagnostics AND (MemDiag # NIL)
   THEN
      CheckParam (paramno) ;
      CASE MemDiag^.type OF

      memdiag:  DEC (MemDiag^.mdiag.param[paramno], VAL (LONGCARD, decr))

      ELSE
         HALT
      END
   END
END MemDecr ;


(*
   MemSet - allow the appropriate parameter to be set to value.
            All parameters are initially set to zero.
*)

PROCEDURE MemSet (MemDiag: Diagnostic; paramno: CARDINAL; value: CARDINAL) ;
BEGIN
   IF EnableDiagnostics AND (MemDiag # NIL)
   THEN
      CheckParam (paramno) ;
      CASE MemDiag^.type OF

      memdiag:  MemDiag^.mdiag.param[paramno] := VAL (LONGCARD, value)

      ELSE
         HALT
      END
   END
END MemSet ;


(*
   TotalHeapIncr - increments the total heap used.
*)

PROCEDURE TotalHeapIncr (incr: CARDINAL) ;
BEGIN
   IF EnableDiagnostics
   THEN
      TotalHeap := TotalHeap + VAL (LONGCARD, incr)
   END
END TotalHeapIncr ;


(*
   TotalHeapDecr - decrements the total heap used.
*)

PROCEDURE TotalHeapDecr (incr: CARDINAL) ;
BEGIN
   IF EnableDiagnostics
   THEN
      TotalHeap := TotalHeap - VAL (LONGCARD, incr)
   END
END TotalHeapDecr ;


(*
   SetEnable - set the enable flag in Diag to value.
*)

PROCEDURE SetEnable (Diag: Diagnostic; value: BOOLEAN) ;
BEGIN
   IF EnableDiagnostics AND (Diag # NIL)
   THEN
      Diag^.enable := value
   END
END SetEnable ;


(*
   Lookup - returns the Diagnostic containing name or NIL
            if it does not exist.
*)

PROCEDURE Lookup (name: ARRAY OF CHAR) : Diagnostic ;
VAR
   ptr: Diagnostic ;
   s  : String ;
BEGIN
   IF EnableDiagnostics
   THEN
      s := InitString (name) ;
      ptr := Head ;
      WHILE ptr # NIL DO
         IF Equal (ptr^.name, s)
         THEN
            s := KillString (s) ;
            RETURN ptr
         END ;
         ptr := ptr^.next
      END ;
      s := KillString (s) ;
      RETURN NIL
   ELSE
      RETURN NIL
   END
END Lookup ;


(*
   ForeachDiagDo - for diag in global diag list do
                      dp (diag);
                   end
*)

PROCEDURE ForeachDiagDo (dp: DiagProc) ;
VAR
   ptr: Diagnostic ;
BEGIN
   ptr := Head ;
   WHILE ptr # NIL DO
      dp (ptr) ;
      ptr := ptr^.next
   END
END ForeachDiagDo ;


(*
   SetDefaultConfig - force the Diag enable flag to the
                      time or mem global default.
*)

PROCEDURE SetDefaultConfig (Diag: Diagnostic) ;
BEGIN
   IF Diag^.type = timediag
   THEN
      Diag^.enable := DefaultTimeEnable
   ELSE
      Diag^.enable := DefaultMemEnable
   END
END SetDefaultConfig ;


(*
   Configure - will turn on or off all the memory or time
               instrumentation diagnostics and set the defaults
               time and mem values.
*)

PROCEDURE Configure (time, mem: BOOLEAN) ;
BEGIN
   IF EnableDiagnostics
   THEN
      DefaultTimeEnable := time ;
      DefaultMemEnable := mem ;
      ForeachDiagDo (SetDefaultConfig)
   END
END Configure ;


(*
   CreateStartTime -
*)

PROCEDURE CreateStartTime ;
BEGIN
   IF EnableDiagnostics
   THEN
      IF StartTime = NIL
      THEN
         StartTime := InitTime (0, 0) ;
         IF GetTimeOfDay (StartTime) = 0
         THEN
         END
      END ;
      IF TotalTime = NIL
      THEN
         TotalTime := InitTime (0, 0)
      END
   ELSE
      StartTime := NIL ;
      TotalTime := NIL
   END
END CreateStartTime ;


(*
   UpdateTotalTime -
*)

PROCEDURE UpdateTotalTime ;
BEGIN
   IF GetTimeOfDay (TotalTime) = 0
   THEN
   END ;
   DecTime (TotalTime, StartTime)
END UpdateTotalTime ;


(*
   GetTimeParam - a paramno of 0 will return the total time so far
                  whereas a paramno > 0 will return the time associated
                  with Diag.
*)

PROCEDURE GetTimeParam (Diag: Diagnostic; paramno: CARDINAL) : Timeval ;
VAR
   sec, usec: CARDINAL ;
BEGIN
   IF paramno = 0
   THEN
      UpdateTotalTime ;
      RETURN TotalTime
   ELSE
      RETURN Diag^.tdiag.total
   END
END GetTimeParam ;


(*
   GetMemParam - return the mem paramno from within Diag.  A paramno of 0
                 will return the total heap.
*)

PROCEDURE GetMemParam (Diag: Diagnostic; paramno: CARDINAL) : LONGCARD ;
BEGIN
   IF paramno = 0
   THEN
      RETURN TotalHeap
   ELSE
      RETURN Diag^.mdiag.param[paramno]
   END
END GetMemParam ;


(*
   CreateDecimalMem - converts c to a decimal string.
*)

PROCEDURE CreateDecimalMem (c: LONGCARD) : String ;
BEGIN
   RETURN LongCardinalToString (c, 0, ' ', 10, TRUE)
END CreateDecimalMem ;


(*
   CreateHexadecimalMem - converts c to a hexadecimal string.
*)

PROCEDURE CreateHexadecimalMem (c: LONGCARD) : String ;
BEGIN
   RETURN ConCat (InitString ('0x'),
                  Mark (LongCardinalToString (c, 0, ' ', 16, TRUE)))
END CreateHexadecimalMem ;


(*
   CreateDecimalTime - return timeval as a decimal seconds.usecs string.
*)

PROCEDURE CreateDecimalTime (timeval: Timeval) : String ;
VAR
   sec, usec: CARDINAL ;
BEGIN
   GetTime (timeval, sec, usec) ;
   RETURN ConCat (ConCat (LongCardinalToString (sec, 0, ' ', 10, TRUE),
                          Mark (InitString ('.'))),
                  LongCardinalToString (usec, 6, '0', 10, TRUE))
END CreateDecimalTime ;


(*
   CreateHexadecimalTime - return timeval as a hexadecimal seconds.usecs string.
*)

PROCEDURE CreateHexadecimalTime (timeval: Timeval) : String ;
VAR
   sec, usec: CARDINAL ;
BEGIN
   GetTime (timeval, sec, usec) ;
   RETURN ConCat (ConCat (LongCardinalToString (sec, 0, ' ', 16, TRUE),
                          Mark (InitString ('.'))),
                  LongCardinalToString (usec, 5, '0', 16, TRUE))
END CreateHexadecimalTime ;


(*
   Decimal - convert paramno in Diag to a string.
*)

PROCEDURE Decimal (Diag: Diagnostic; paramno: CARDINAL) : String ;
BEGIN
   CASE Diag^.type OF

   memdiag :  RETURN CreateDecimalMem (GetMemParam (Diag, paramno)) |
   timediag:  RETURN CreateDecimalTime (GetTimeParam (Diag, paramno))

   END ;
   RETURN NIL
END Decimal ;


(*
   Hexadecimal - convert paramno in Diag to a hex string.
*)

PROCEDURE Hexadecimal (Diag: Diagnostic; paramno: CARDINAL) : String ;
BEGIN
   CASE Diag^.type OF

   memdiag :  RETURN CreateHexadecimalMem (GetMemParam (Diag, paramno)) |
   timediag:  RETURN CreateHexadecimalTime (GetTimeParam (Diag, paramno))

   END ;
   RETURN NIL
END Hexadecimal ;


(*
   Count - return the count field for a time diag or return the decimal
           value for a paramno in a mem diag.
*)

PROCEDURE Count (Diag: Diagnostic; paramno: CARDINAL) : String ;
BEGIN
   CASE Diag^.type OF

   memdiag :  RETURN CreateDecimalMem (GetMemParam (Diag, paramno)) |
   timediag:  RETURN ctos (Diag^.tdiag.count, 0, ' ')

   END ;
   RETURN NIL
END Count ;


(*
   Microsec - convert timeval into microseconds and return the value as
              a longcard.
*)

PROCEDURE Microsec (timeval: Timeval) : LONGCARD ;
VAR
   sec, usec: CARDINAL ;
   microsec : LONGCARD ;
BEGIN
   GetTime (timeval, sec, usec) ;
   microsec := VAL (LONGCARD, sec) * MICROSEC + VAL (LONGCARD, usec) ;
   RETURN microsec
END Microsec ;


(*
   CreateTimePercent - return timeval as a percentage of the TotalTime.
*)

PROCEDURE CreateTimePercent (timeval: Timeval) : String ;
VAR
   total, param: LONGCARD ;
BEGIN
   IF timeval = TotalTime
   THEN
      param := 100
   ELSE
      UpdateTotalTime ;
      param := Microsec (timeval) * 100 ;
      total := Microsec (TotalTime) ;
      IF total = 0
      THEN
         param := 0
      ELSE
         param := param DIV total
      END
   END ;
   RETURN ConCatChar (ctos (VAL (CARDINAL, param), 3, ' '), '%')
END CreateTimePercent ;


(*
   CreateMemPercent - return memval as a percentage of TotalHeap.
*)

PROCEDURE CreateMemPercent (memval: LONGCARD) : String ;
VAR
   param: LONGCARD ;
BEGIN
   IF memval = TotalHeap
   THEN
      param := 100
   ELSE
      param := memval * 100 ;
      IF TotalHeap = 0
      THEN
         param := 0
      ELSE
         param := param DIV TotalHeap
      END
   END ;
   RETURN ConCatChar (ctos (VAL (CARDINAL, param), 3, ' '), '%')
END CreateMemPercent ;


(*
   DescribePercent - call the appropriate mem or time percentage procedure.
*)

PROCEDURE DescribePercent (Diag: Diagnostic; paramno: CARDINAL) : String ;
BEGIN
   CASE Diag^.type OF

   memdiag :  RETURN CreateMemPercent (GetMemParam (Diag, paramno)) |
   timediag:  RETURN CreateTimePercent (GetTimeParam (Diag, paramno))

   END ;
   RETURN NIL
END DescribePercent ;


(*
   DescribeMemory - return the memory diagnostic
*)

PROCEDURE DescribeMemory (Diag: Diagnostic; paramno: CARDINAL) : String ;
CONST
   kilo = 1024 ;
   mega = kilo * kilo ;
   giga = mega * kilo ;
VAR
   param: LONGCARD ;
   s    : String ;
BEGIN
   param := GetMemParam (Diag, paramno) ;
   IF param < kilo
   THEN
      s := ConCat (LongCardinalToString (param, 0, ' ', 10, FALSE),
                   Mark (InitString (' Bytes')))
   ELSIF param < mega
   THEN
      param := param DIV kilo ;
      s := ConCat (LongCardinalToString (param, 0, ' ', 10, FALSE),
                   Mark (InitString ('KB')))
   ELSIF param < giga
   THEN
      param := param DIV mega ;
      s := ConCat (LongCardinalToString (param, 0, ' ', 10, FALSE),
                   Mark (InitString ('MB')))
   ELSE
      param := param DIV giga ;
      s := ConCat (LongCardinalToString (param, 0, ' ', 10, FALSE),
                   Mark (InitString ('GB')))
   END ;
   RETURN s
END DescribeMemory ;


(*
   DescribeTime - returns the time diagnostic in seconds.
*)

PROCEDURE DescribeTime (Diag: Diagnostic; paramno: CARDINAL) : String ;
VAR
   sec, usec: CARDINAL ;
BEGIN
   CASE Diag^.type OF

   memdiag :  HALT |
   timediag:  GetTime (GetTimeParam (Diag, paramno), sec, usec) ;
              RETURN ConCat (ConCat (LongCardinalToString (sec, 0, ' ', 10, TRUE),
                                     Mark (InitString ('.'))),
                             ConCat (LongCardinalToString (usec, 6, '0', 10, TRUE),
                                     Mark (InitString (' sec'))))

   END ;
   RETURN NIL
END DescribeTime ;


(*
   ParamSpec - ebnf:

               ( '{' | '0' | '1' | '2' | '3' | '4' )
               ( 'd' | 'x' | 'C' | 'H' | 'T' | 'M' | 'N' | 'P' )
               '}'
*)

PROCEDURE ParamSpec (Diag: Diagnostic; i: CARDINAL) : CARDINAL ;
VAR
   paramno,
   length : CARDINAL ;
   ch     : CHAR ;
BEGIN
   length := Length (Diag^.format) ;
   paramno := 0 ;
   IF i < length
   THEN
      ch := char (Diag^.format, i) ;
      CASE ch OF

      '{':  Output := ConCatChar (Output, '{') ;
            RETURN i + 1 |
      '0':  paramno := 0 |
      '1':  paramno := 1 |
      '2':  paramno := 2 |
      '3':  paramno := 3 |
      '4':  paramno := 4

      ELSE
         Error ('unexpected character: ', ch)
      END ;
      INC (i) ;
      IF i < length
      THEN
         ch := char (Diag^.format, i) ;
         CASE ch OF

         'd':  Output := ConCat (Output, Decimal (Diag, paramno)) |
         'x':  Output := ConCat (Output, Hexadecimal (Diag, paramno)) |
         'C':  Output := ConCat (Output, Count (Diag, paramno)) |
         'H':  Output := ConCat (Output, HierarchicalName (Diag, i)) |
         'M':  Output := ConCat (Output, DescribeMemory (Diag, paramno)) |
         'N':  Output := ConCat (Output, Diag^.name) |
         'P':  Output := ConCat (Output, DescribePercent (Diag, paramno)) |
         'T':  Output := ConCat (Output, DescribeTime (Diag, paramno))

         ELSE
            Error ('unexpected character: ', ch)
         END ;
         INC (i) ;
         IF i < length
         THEN
            ch := char (Diag^.format, i) ;
            IF ch # '}'
            THEN
               Error ('expected } character, seen ', ch)
            END
         END
      END
   END ;
   RETURN i + 1
END ParamSpec ;


(*
   HierarchicalName - if the hierarchical formatting of output
                      has been enabled use the last component
                      of the name separated by ':' else
                      output full name.
*)

PROCEDURE HierarchicalName (Diag: Diagnostic; pos: CARDINAL) : String ;
VAR
   i, j: INTEGER ;
BEGIN
   IF EnableHierarchical
   THEN
      i := DynamicStrings.Index (Diag^.name, '}', pos) ;
      IF i > 0
      THEN
         j := i - 1 ;
         i := RIndex (Diag^.name, ':', j) ;
         IF (i >= 0) AND (i < j)
         THEN
            RETURN DynamicStrings.Slice (Diag^.name, i, j)
         END
      END
   END ;
   RETURN Diag^.name
END HierarchicalName ;


(*
   FormatDiag - ebnf:

                { ( '{' ParamSpec ) | any }
*)

PROCEDURE FormatDiag (Diag: Diagnostic) ;
VAR
   i, length: CARDINAL ;
   ch       : CHAR ;
BEGIN
   i := 0 ;
   length := Length (Diag^.format) ;
   WHILE i < length DO
      ch := char (Diag^.format, i) ;
      IF ch = '{'
      THEN
         INC (i) ;
         i := ParamSpec (Diag, i)
      ELSE
         Output := ConCatChar (Output, ch) ;
         INC (i)
      END
   END ;
   Output := ConCatChar (Output, nl)
END FormatDiag ;


(*
   GetName - returns the name of Diag.
*)

PROCEDURE GetName (Diag: Diagnostic) : String ;
BEGIN
   IF EnableDiagnostics AND (Diag # NIL)
   THEN
      RETURN Diag^.name
   ELSE
      RETURN NIL
   END
END GetName ;


(*
   Match -
*)

PROCEDURE Match (stem, name: String) : BOOLEAN ;
BEGIN
   RETURN TRUE
END Match ;


(*
   HierarchicalDiag - iterate over every diagnostic using a depth first search
                      for each component of the diagnostic name.
*)

PROCEDURE HierarchicalDiag (stem: String; visited: Index) : String ;
VAR
   diag: Diagnostic ;
BEGIN
   diag := Head ;
   WHILE diag # NIL DO
      IF NOT Indexing.IsIndiceInIndex (visited, diag)
      THEN
         IF Match (stem, diag^.name)
         THEN
            Indexing.IncludeIndiceIntoIndex (visited, diag)
         END
      END ;
      diag := diag^.next
   END ;
   RETURN Output
END HierarchicalDiag ;


(*
   GenerateRaw - return the output string after calling FormatDiag on
                 every diagnostic rule.
*)

PROCEDURE GenerateRaw () : String ;
BEGIN
   ForeachDiagDo (FormatDiag) ;
   RETURN Output
END GenerateRaw ;


(*
   GenerateHierarchical -
*)

PROCEDURE GenerateHierarchical () : String ;
BEGIN
   RETURN HierarchicalDiag (InitString (''), Indexing.InitIndex (1))
END GenerateHierarchical ;


(*
   Generate - return a string containing the output from
              all the diagnostics enabled.
*)

PROCEDURE Generate (hierarchical: BOOLEAN) : String ;
BEGIN
   EnableHierarchical := hierarchical ;
   IF EnableDiagnostics
   THEN
      Output := KillString (Output) ;
      Output := InitString ('') ;
      IF hierarchical
      THEN
         RETURN GenerateHierarchical ()
      ELSE
         RETURN GenerateRaw ()
      END
   ELSE
      RETURN NIL
   END
END Generate ;


BEGIN
   TotalHeap := 0 ;
   StartTime := NIL ;
   TotalTime := NIL ;
   CreateStartTime ;
   Head := NIL ;
   Output := NIL ;
   EnableHierarchical := FALSE ;
   DefaultTimeEnable := DefaultTimeEnableValue ;
   DefaultMemEnable := DefaultMemEnableValue ;
END M2Diagnostic.
