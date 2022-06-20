(* M2Scaffold.mod declare and create scaffold entities.

Copyright (C) 2022 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Scaffold ;

FROM SymbolTable IMPORT NulSym, MakeProcedure, PutFunction,
                        PutPublic, PutCtor, PutParam, IsProcedure,
                        StartScope,
                        EndScope ;

FROM NameKey IMPORT MakeKey ;
FROM M2Base IMPORT Integer ;
FROM M2System IMPORT Address ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM Assertion IMPORT Assert ;


(* The dynamic scaffold takes the form:

static void _M2_init (int argc, char *argv[], char *envp[])
{
  M2RTS_ConstructModules (module_name, argc, argv, envp);
}


static void _M2_finish (int argc, char *argv[], char *envp[])
{
  M2RTS_Terminate ();
  M2RTS_DeconstructModules (module_name, argc, argv, envp);
}


int
main (int argc, char *argv[], char *envp[])
{
  init (argc, argv, envp);
  finish ();
  return (0);
}  *)


(*
   DeclareScaffoldFunctions - declare main, _M2_init,_M2_finish
                              and _M2_DependencyGraph to the modula-2
                              front end.
*)

PROCEDURE DeclareScaffoldFunctions (tokenno: CARDINAL) ;
BEGIN
   mainFunction := MakeProcedure (tokenno, MakeKey ("main")) ;
   StartScope (mainFunction) ;
   PutFunction (mainFunction, Integer) ;
   DeclareArgEnvParams (tokenno, mainFunction) ;
   PutPublic (mainFunction, TRUE) ;
   EndScope ;

   initFunction := MakeProcedure (tokenno, MakeKey ("_M2_init")) ;
   DeclareArgEnvParams (tokenno, initFunction) ;

   finiFunction := MakeProcedure (tokenno, MakeKey ("_M2_finish")) ;
   DeclareArgEnvParams (tokenno, finiFunction)
END DeclareScaffoldFunctions ;


(*
   DeclareArgEnvParams - declares (int argc, void *argv, void *envp)
*)

PROCEDURE DeclareArgEnvParams (tokno: CARDINAL; proc: CARDINAL) ;
BEGIN
   Assert (IsProcedure (proc)) ;
   StartScope (proc) ;
   Assert (PutParam (tokno, proc, 1, MakeKey ("argc"), Integer, FALSE)) ;
   Assert (PutParam (tokno, proc, 2, MakeKey ("argv"), Address, FALSE)) ;
   Assert (PutParam (tokno, proc, 3, MakeKey ("envp"), Address, FALSE)) ;
   EndScope
END DeclareArgEnvParams ;



(*
   DeclareScaffold - declare scaffold related entities.
*)

PROCEDURE DeclareScaffold (tokno: CARDINAL) ;
BEGIN
   DeclareScaffoldFunctions (tokno)
END DeclareScaffold ;


BEGIN
   finiFunction := NulSym ;
   initFunction := NulSym ;
   mainFunction := NulSym
END M2Scaffold.
