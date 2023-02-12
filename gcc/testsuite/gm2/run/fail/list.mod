(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE list ;


FROM FIO IMPORT OpenToRead, OpenToWrite, Close, EOF, WriteLine,
                ReadString, WriteString, File, WriteChar;
FROM NumberIO IMPORT WriteInt,ReadInt,StrToInt,IntToStr;
FROM FpuIO IMPORT ReadReal,WriteReal,StrToReal,RealToStr;
FROM StdIO IMPORT Write;
IMPORT StrIO; (* WriteString,WriteLn,ReadString; *)
FROM StrLib IMPORT StrLen,StrLess,StrEqual;

TYPE
    string4  = ARRAY[0..3] OF CHAR;
    string8 = ARRAY[0..7] OF CHAR;
    string10 = ARRAY[0..9] OF CHAR;
    string20 = ARRAY[0..19] OF CHAR;
    string40 = ARRAY[0..39] OF CHAR;

   employeeRecordType = RECORD
          Forename : string20;
          Surname  : string40;
          EmpNumber: string10;
          Address1 : string20;
          Address2 : string20;
          Address3 : string20;
          Address4 : string20;
          END;(*record*)

VAR
   filehandle: File ;
   crlf      : ARRAY[0..1] OF CHAR;


PROCEDURE getRecord(VAR employeeRec: employeeRecordType);
BEGIN
   WITH employeeRec DO
      (*Read in each field in turn.*)
      ReadString(filehandle,Surname);
      ReadString(filehandle,Forename);
      ReadString(filehandle,EmpNumber);
      ReadString(filehandle,Address1);
      ReadString(filehandle,Address2);
      ReadString(filehandle,Address3);
      ReadString(filehandle,Address4);
      ReadString(filehandle,crlf);
   END
END getRecord; 


PROCEDURE getFile (nameOfFile : ARRAY OF CHAR) ;
VAR
    inRec : employeeRecordType;
BEGIN
    (*Reset the file to read it all in.*)
    filehandle := OpenToRead(nameOfFile) ;

    (*Process all the lines in the file.*)
    WHILE NOT EOF(filehandle ) DO 
    getRecord(inRec); 
        printEntry(inRec);
    END ; (* WHILE NOT EOF() *)
    Close(filehandle);
END getFile ; 

PROCEDURE printEntry(which : employeeRecordType) ;
BEGIN
    WITH which DO
        (*print each field in turn.*)
        StrIO.WriteString(Forename);
    StrIO.WriteString(" ");
    StrIO.WriteString(Surname);
    StrIO.WriteString(" ");
    StrIO.WriteString(EmpNumber);
    StrIO.WriteLn;
    StrIO.WriteString(Address1);
    StrIO.WriteLn;
    StrIO.WriteString(Address2);
    StrIO.WriteLn;
    StrIO.WriteString(Address3);
    StrIO.WriteLn;
    StrIO.WriteString(Address4);
    END ; (*with*)
StrIO.WriteLn;
END printEntry; (*printEntry*)


BEGIN
   getFile('employee.txt')
END list.
