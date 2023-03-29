(* gm2lorder.mod ensure that underlying runtime modules are initialized.

Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

MODULE gm2lorder ;

(*
   Author     : Gaius Mulley
   Title      : gm2lorder
   Date       : Thu Sep  4 21:18:33 BST 2008
   Description: Ensures that underlying runtime modules are initialized
                before all other modules.
*)


FROM libc IMPORT exit ;
FROM ASCII IMPORT eof ;
FROM SArgs IMPORT GetArg ;
FROM StrLib IMPORT StrLen ;
FROM DynamicStrings IMPORT String, InitString, KillString, Length, Equal, EqualArray, Slice, Mark ;
FROM Indexing IMPORT Index, PutIndice, GetIndice, RemoveIndiceFromIndex, InitIndex, KillIndex, HighIndice ;
FROM FIO IMPORT File, StdIn, StdOut, StdErr, WriteChar, WriteLine,
                Close, EOF, IsNoError, WriteString, FlushBuffer ;
FROM SFIO IMPORT OpenToRead, OpenToWrite, ReadS, WriteS ;

IMPORT DynamicStrings ;


CONST
   Comment = '#' ;               (* Comment identifier.     *)
   DefaultRuntimeModules = 'Storage,SYSTEM,M2RTS,RTExceptions,IOLink' ;

VAR
   fi, fo    : File ;
   runTime   : Index ;
   moduleList: Index ;


(*
   InitRuntimeModules - initializes the list of critical runtime modules
                        which must be initialized first and in a particular
                        order.
*)

PROCEDURE InitRuntimeModules (s: String) ;
VAR
   a   : CARDINAL ;
   i, j: INTEGER ;
BEGIN
   IF runTime # NIL
   THEN
      runTime := KillIndex (runTime)
   END ;
   runTime := InitIndex (0) ;
   i := 0 ;
   a := 0 ;
   REPEAT
      j := DynamicStrings.Index (s, ',', i) ;
      IF j = -1
      THEN
         PutIndice (runTime, a, Slice (s, i, 0))
      ELSE
         PutIndice (runTime, a, Slice (s, i, j)) ;
         i := j+1
      END ;
      INC(a)
   UNTIL j=-1 ;
   s := KillString (s)
END InitRuntimeModules ;


(*
   Reorder - reorders the list of modules to ensure critical runtime
             modules are initialized first.  It writes out the new
             ordered list.
*)

PROCEDURE Reorder ;
VAR
   rh, mh,
   ri, mi: CARDINAL ;
   rs, ms: String ;
BEGIN
   rh := HighIndice (runTime) ;
   mh := HighIndice (moduleList) ;
   ri := 0 ;
   WHILE ri <= rh DO
      mi := 0 ;
      rs := GetIndice (runTime, ri) ;
      WHILE mi <= mh DO
         ms := GetIndice (moduleList, mi) ;
         IF Equal (rs, ms)
         THEN
            rs := WriteS (fo, rs) ; WriteLine (fo) ;
            RemoveIndiceFromIndex (moduleList, ms) ;
            mh := HighIndice (moduleList)
         ELSE
            INC (mi)
         END
      END ;
      INC (ri)
   END ;
   mi := 0 ;
   WHILE mi <= mh DO
      ms := GetIndice (moduleList, mi) ;
      ms := WriteS (fo, ms) ; WriteLine (fo) ;
      INC (mi)
   END ;
   Close (fo)
END Reorder ;


(*
   ReadList - populates the moduleList with a list of module names.
*)

PROCEDURE ReadList ;
VAR
   s: String ;
   i: CARDINAL ;
BEGIN
   moduleList := InitIndex (0) ;
   i := 0 ;
   s := ReadS (fi) ;
   WHILE NOT EOF (fi) DO
      IF NOT EqualArray (s, '')
      THEN
         PutIndice (moduleList, i, s) ;
         INC (i)
      END ;
      s := ReadS (fi)
   END ;
   IF NOT EqualArray (s, '')
   THEN
      PutIndice (moduleList, i, s)
   END
END ReadList ;


(*
   OpenOutputFile - attempts to open an output file.
*)

PROCEDURE OpenOutputFile (s: String) ;
BEGIN
   IF EqualArray(s, '-')
   THEN
      fo := StdOut
   ELSE
      fo := OpenToWrite(s) ;
      IF NOT IsNoError(fo)
      THEN
         WriteString(StdErr, 'cannot write to: ') ; s := WriteS(StdErr, s) ; WriteLine(StdErr) ;
         exit(1)
      END
   END
END OpenOutputFile ;


(*
   Usage - prints out a usage and exits with 0.
*)

PROCEDURE Usage ;
BEGIN
   WriteString(StdOut, 'gm2lorder [-h] [-o outputfile] [-fruntime-modules=] inputfile') ; WriteLine(StdOut) ;
   WriteString(StdOut, '    inputfile is a file containing a list of modules, each module on a separate line') ; WriteLine(StdOut) ;
   WriteString(StdOut, '    the list of runtime modules can be specified as follows -fruntime-modules=module1,module2,module3') ; WriteLine(StdOut) ;
   WriteString(StdOut, '    the default for this flag is -fruntime-modules=') ;
   WriteString(StdOut, DefaultRuntimeModules) ; WriteLine(StdOut) ;
   WriteString(StdOut, '    Note that the list of runtime modules does not mean they will appear in the executable') ; WriteLine(StdOut) ;
   WriteString(StdOut, '    a runtime module is only included into the final executable if it is required,') ; WriteLine(StdOut) ;
   WriteString(StdOut, '    however gm2lorder will ensure the order of these modules.') ; WriteLine(StdOut) ;
   FlushBuffer(StdOut) ;
   exit(0)
END Usage ;


(*
   ScanArgs - scans arguments.
*)

PROCEDURE ScanArgs ;
VAR
   i        : CARDINAL ;
   s        : String ;
   FoundFile: BOOLEAN ;
BEGIN
   FoundFile := FALSE ;
   fi        := StdIn ;
   fo        := StdOut ;
   i := 1 ;
   WHILE GetArg(s, i) DO
      IF EqualArray(s, '-o')
      THEN
         s := KillString(s) ;
         INC(i) ;
         IF GetArg(s, i)
         THEN
            OpenOutputFile(s)
         ELSE
            WriteString(StdErr, 'missing filename option after -o') ; WriteLine(StdErr) ;
            exit(1)
         END
      ELSIF EqualArray(s, '-h')
      THEN
         Usage
      ELSIF EqualArray(Mark(Slice(s, 0, StrLen('-fruntime-modules='))), '-fruntime-modules=')
      THEN
         InitRuntimeModules(Slice(s, StrLen('-fruntime-modules='), 0))
      ELSE
         IF FoundFile
         THEN
            WriteString(StdErr, 'already opened one file for reading') ; WriteLine(StdErr)
         ELSE
            FoundFile := TRUE ;
            fi := OpenToRead(s) ;
            IF NOT IsNoError(fi)
            THEN
               WriteString(StdErr, 'failed to open: ') ; s := WriteS(StdErr, s) ; WriteLine(StdErr) ;
               exit(1)
            END
         END
      END ;
      INC(i)
   END ;
   IF NOT FoundFile
   THEN
      WriteString(StdErr, 'a module file list must be specified on the command line') ; WriteLine(StdErr) ;
      exit(1)
   END
END ScanArgs ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   runTime := NIL ;
   moduleList := NIL ;
   InitRuntimeModules(InitString(DefaultRuntimeModules)) ;
   ScanArgs ;
   ReadList ;
   Reorder
END Init ;


BEGIN
   Init
END gm2lorder.
