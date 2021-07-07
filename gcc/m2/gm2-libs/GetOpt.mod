(* GetOpt.mod allows users to manage long options to getopt.

Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE GetOpt ;  (*!m2pim+gm2*)

FROM DynamicStrings IMPORT string, InitStringCharStar ;
FROM Storage IMPORT ALLOCATE, REALLOCATE, DEALLOCATE ;
FROM MemUtils IMPORT MemCopy ;

IMPORT getopt ;


TYPE
   Crecord = RECORD    (* see man 3 getopt.  *)
                name   : ADDRESS ;
                has_arg: INTEGER ;
                flag   : PtrToInteger ;
                val    : INTEGER ;
             END ;

   ptrToCrecord = POINTER TO Crecord ;

   LongOptions = POINTER TO RECORD
                               cptr: ptrToCrecord ;
                               len : CARDINAL ;
                               size: CARDINAL ;
                            END ;


(*
   GetOpt - call C getopt and fill in the parameters:
            optarg, optind, opterr and optop.
*)

PROCEDURE GetOpt (argc: INTEGER; argv: ADDRESS; optstring: String;
                  VAR optarg: String;
                  VAR optind, opterr, optopt: INTEGER) : CHAR ;
VAR
   r: CHAR ;
BEGIN
   r := getopt.getopt (argc, argv, string (optstring)) ;
   optarg := InitStringCharStar (getopt.optarg) ;
   opterr := getopt.opterr ;
   optopt := getopt.optopt ;
   RETURN r
END GetOpt ;


(*
   InitLongOptions - creates and returns a LongOptions empty array.
*)

PROCEDURE InitLongOptions () : LongOptions ;
VAR
   lo: LongOptions ;
BEGIN
   NEW (lo) ;
   WITH lo^ DO
      cptr := NIL ;
      len := 0 ;
      size := 0
   END ;
   RETURN lo
END InitLongOptions ;


(*
   AddLongOption - appends long option {name, has_arg, flag, val} to the
                   array of options and new long options array is returned.
                   The old array, lo, should no longer be used.

   (from man 3 getopt)
       The meanings of the different fields are:

       name   is the name of the long option.

       has_arg
              is: no_argument (or 0) if the option does not take an  argument;  required_argument
              (or  1)  if  the  option  requires  an argument; or optional_argument (or 2) if the
              option takes an optional argument.

       flag   specifies how results are returned for a  long  option.   If  flag  is  NULL,  then
              getopt_long()  returns  val.   (For example, the calling program may set val to the
              equivalent short option character.)  Otherwise, getopt_long() returns 0,  and  flag
              points to a variable which is set to val if the option is found, but left unchanged
              if the option is not found.

       val    is the value to return, or to load into the variable pointed to by flag.

       The last element of the array has to be filled with zeros.
*)

PROCEDURE AddLongOption (lo: LongOptions;
                         name: String; has_arg: INTEGER;
                         flag: PtrToInteger; val: INTEGER) : LongOptions ;
VAR
   old,
   entry: ptrToCrecord ;
BEGIN
   IF lo^.cptr = NIL
   THEN
      NEW (lo^.cptr) ;
      lo^.len := 1 ;
      lo^.size := SIZE (Crecord) ;
      entry := lo^.cptr
   ELSE
      old := lo^.cptr ;
      INC (lo^.len) ;
      lo^.size := lo^.len * SIZE (Crecord) ;
      REALLOCATE (lo^.cptr, lo^.size) ;
      IF lo^.cptr = NIL
      THEN
         entry := NIL
      ELSIF old = lo^.cptr
      THEN
         entry := lo^.cptr ;
         INC (entry, SIZE (Crecord) * lo^.len-1)
      ELSE
         MemCopy (old, lo^.len-1, lo^.cptr) ;
         entry := lo^.cptr ;
         INC (entry, SIZE (Crecord) * lo^.len-1)
      END
   END ;
   fillIn (entry, name, has_arg, flag, val) ;
   RETURN lo
END AddLongOption ;


(*
   fillIn - fills in
*)

PROCEDURE fillIn (entry: ptrToCrecord;
                  name: String; has_arg: INTEGER; flag: PtrToInteger; val: INTEGER) ;
BEGIN
   IF entry # NIL
   THEN
      entry^.name := name ;
      entry^.has_arg := has_arg ;
      entry^.flag := flag ;
      entry^.val := val
   END
END fillIn ;


(*
   KillLongOptions - returns NIL and also frees up memory associated with, lo.
*)

PROCEDURE KillLongOptions (lo: LongOptions) : LongOptions ;
BEGIN
   DEALLOCATE (lo^.cptr, lo^.size) ;
   DISPOSE (lo) ;
   RETURN NIL
END KillLongOptions ;


(*
   GetOptLong - works like GetOpt but will accept long options (using two dashes).
                If the program only accepts long options then optstring should be
                an empty string, not NIL.
*)

PROCEDURE GetOptLong (argc: INTEGER; argv: ADDRESS; optstring: String;
                      longopts: LongOptions; VAR longindex: INTEGER) : INTEGER ;
VAR
   r: INTEGER ;
BEGIN
   r := getopt.getopt_long (argc, argv, string (optstring), longopts^.cptr, longindex) ;
   RETURN r
END GetOptLong ;


(*
   GetOptLongOnly - works like GetOptLong except that a single dash can be used
                    for a long option.
*)

PROCEDURE GetOptLongOnly (argc: INTEGER; argv: ADDRESS; optstring: String;
                          longopts: LongOptions; VAR longindex: INTEGER) : INTEGER ;
VAR
   r: INTEGER ;
BEGIN
   r := getopt.getopt_long_only (argc, argv, string (optstring),
                                 longopts^.cptr, longindex) ;
   RETURN r
END GetOptLongOnly ;


END GetOpt.
