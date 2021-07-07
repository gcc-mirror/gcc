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

MODULE prog21;

(* Referral exercise from 1995 *)

(* Author - Stuart F Lewis March 1997 *)

(* Reads student sports race data from a file *)
(* stores in an array*)
 (* displays the results *)

(* MODEL SOLUTION FOR prog21 for course.swd *)

FROM StrIO IMPORT WriteLn,WriteString,ReadString;
FROM StrLib IMPORT StrEqual, StrLess, StrConCat;
FROM StdIO IMPORT Read, Write;
FROM NumberIO IMPORT WriteInt, ReadCard,WriteCard;
FROM ASCII IMPORT eof;

CONST
   MaxPeople = 20;
   EndOfRaceMark = 'qqqqqqqqqq';

TYPE
   Rec = RECORD
            strdata : ARRAY[0..9] OF CHAR;
         END;

   name_type = ARRAY[0..9] OF CHAR;
   OnePerson = RECORD
                  name : name_type;
                  score : CARDINAL;
               END;

   All = ARRAY[1..MaxPeople] OF OnePerson;

VAR
   draw,position,Npeople,count : CARDINAL;
   Results : All;

PROCEDURE continue;
VAR
   ch : CHAR;
BEGIN
   WriteLn;
   WriteString("press ENTER to continue > ");
   Read(ch);
END continue;

(* main program procedures start here *)

PROCEDURE Initialise;
BEGIN
   Npeople := 0;
   FOR count :=1 TO MaxPeople DO
      WITH Results[count] DO
         name := '          ';
         score := 0;
      END;(*with*)
   END;(*for*)
END Initialise;

PROCEDURE SearchFor(temp : name_type;in : All ;
                    VAR Index : CARDINAL) : BOOLEAN ;
BEGIN
   FOR count :=1 TO Npeople DO
      WITH in[count] DO
         IF StrEqual(name,temp)
         THEN
            Index := count;
            RETURN TRUE;
         END;(*if*)
      END;(*with*)
   END;(*for*)
   RETURN FALSE;
END SearchFor;

PROCEDURE Add(temp : name_type;VAR in : All ; VAR Index : CARDINAL) ;
BEGIN
   Npeople := Npeople +1;
   WITH in[Npeople] DO
      name := temp;
   END;(*with*)
   Index := Npeople;
END Add;

PROCEDURE ReadStr10(VAR temp : name_type) ;
BEGIN
   temp := '          ';
   ReadString(temp);
   StrConCat(temp,'         ',temp);
   WriteString(temp);
   WriteString('****');
   WriteLn;
END ReadStr10;


PROCEDURE InputData;
VAR
  race,temp : name_type;
  Index,points : CARDINAL;
BEGIN
   ReadStr10(race);
   WHILE NOT StrEqual(race,EndOfRaceMark) DO
      ReadStr10(temp); (*blank line*)
      ReadStr10(temp); (*first person in a race*)
      points := 3; (*first place*)
      WHILE StrEqual(temp,EndOfRaceMark) DO
            IF NOT SearchFor(temp,Results,Index) THEN
                   Add(temp,Results,Index);
            END;(*if*)
            IF points > 0 THEN
               Results[Index].score := Results[Index].score + points;
               points := points - 1;
            END;(*if*)
            ReadStr10(temp);
      END;(*while same race*)
      (* ReadStr10(temp); blank line*)
      ReadStr10(race);
   END;(*while races*)
   WriteString(race);
END InputData;

PROCEDURE OutputResults;
BEGIN
   position := 1;
   draw := 0;
   FOR count :=1 TO Npeople DO
      WITH Results[count] DO
         WriteString(name);
         WriteCard(score,6);
         WriteCard(position,4);
         IF score > Results[count + 1].score
         THEN
            position := position + 1 + draw;
            IF draw > 0
            THEN
               draw := 0;
               WriteString("=");
            END;(*if*)
         END;(*if*)
         IF (score = Results[count + 1].score)
         THEN
            draw := draw + 1;
            WriteString("=");
         END;(*if*)
         WriteLn;
      END;(*with*)
   END;(*for*)
END OutputResults;

PROCEDURE Less(first,second : CARDINAL) : BOOLEAN;
BEGIN
   (*compares score first then name to maintain order *)
   (* comparison is back to front because we want descending order*)
   IF Results[first].score > Results[second].score
   THEN RETURN TRUE
   ELSE
      IF (Results[first].score = Results[second].score)
           AND (StrLess(Results[first].name,Results[second].name))
      THEN RETURN TRUE
      ELSE RETURN FALSE
      END;(*IF*)
END;(*IF*)
END Less;

PROCEDURE Swap(first,second : CARDINAL);
VAR
   temp : OnePerson;
BEGIN
   temp := Results[first];
   Results[first] := Results[second];
   Results[second] := temp;
END Swap;


BEGIN (*main program*)
   Initialise;
   InputData;
   WriteLn;
   (*HSort(Npeople,Less,Swap);*)
   OutputResults;
END prog21.
