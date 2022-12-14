(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

MODULE tinyvarient6 ;   (*!m2pim*)


CONST
   MaxCodeHunkLength = 4 ;

TYPE
   Name = CARDINAL ;

   ElementType = (idel, tokel, litel) ;

   m2condition = (m2none, m2if, m2elsif, m2while) ;

   TraverseResult = (unknown, true, false) ;

   ProductionDesc = POINTER TO productiondesc ;

   IdentDesc      = POINTER TO identdesc ;   (* forward fodder for p2c *)
   identdesc      =            RECORD
                                  definition: ProductionDesc ;   (* where this idents production is defined *)
                                  name      : Name ;
                                  line      : CARDINAL ;
                               END ;

   SetDesc        = POINTER TO setdesc ;
   setdesc        =            RECORD
                                  next          : SetDesc ;
                                  CASE type: ElementType OF

                                  idel  : ident : IdentDesc |
                                  tokel,
                                  litel : string: Name

                                  END
                               END ;

(* note that epsilon refers to whether we can satisfy this component part
   of a sentance without consuming a token. Reachend indicates we can get
   to the end of the sentance without consuming a token.

   For expression, statement, productions, terms: the epsilon value should
   equal the reachend value but for factors the two may differ.
*)

   FollowDesc     = POINTER TO followdesc ;
   followdesc     =            RECORD
                                  calcfollow  : BOOLEAN ;          (* have we solved the follow set yet? *)
                                  follow      : SetDesc ;          (* the follow set *)
                                  reachend    : TraverseResult ;   (* can we see the end of the sentance (due to multiple epsilons) *)
                                  epsilon     : TraverseResult ;   (* potentially no token may be consumed within this component of the sentance *)
                                  line        : CARDINAL ;
                    END ;

   TermDesc       = POINTER TO termdesc ;

   ExpressionDesc = POINTER TO expressiondesc ;
   expressiondesc =            RECORD
                                  term      : TermDesc ;
                                  followinfo: FollowDesc ;
                                  line      : CARDINAL ;
                               END ;

   StatementDesc  = POINTER TO statementdesc ;
   statementdesc  =            RECORD
                                  ident      : IdentDesc ;
                                  expr       : ExpressionDesc ;
                                  followinfo : FollowDesc ;
                                  line       : CARDINAL ;
                               END ;

   CodeHunk       = POINTER TO codehunk ;
   codehunk       =            RECORD
                                  codetext  : ARRAY [0..MaxCodeHunkLength] OF CHAR ;
                                  next      : CodeHunk ;
                               END ;

   CodeDesc       = POINTER TO codedesc ;
   codedesc       =            RECORD
                                  code      : CodeHunk ;
                                  indent    : CARDINAL ;         (* column of the first % *)
                                  line      : CARDINAL ;
                               END ;

   FactorType     = (id, lit, sub, opt, mult, m2) ;

   FactorDesc     = POINTER TO factordesc ;
   factordesc     =            RECORD
                                  followinfo: FollowDesc ;
                                  next      : FactorDesc ;   (* chain of successive factors *)
                                  line      : CARDINAL ;
                                  pushed    : FactorDesc ;   (* chain of pushed code factors *)
                                  CASE type: FactorType OF

                                  id  : ident : IdentDesc |
                                  lit : string: Name |
                                  sub,
                                  opt,
                                  mult: expr  : ExpressionDesc |
                                  m2  : code  : CodeDesc ;

                                  END
                               END ;

   termdesc       =            RECORD
                                  factor    : FactorDesc ;
                                  next      : TermDesc ;  (* chain of alternative terms *)
                                  followinfo: FollowDesc ;
                                  line      : CARDINAL ;
                               END ;

   productiondesc =            RECORD
                                  next        : ProductionDesc ;   (* the chain of productions *)
                                  statement   : StatementDesc ;
                                  first       : SetDesc ;          (* the first set *)
                                  firstsolved : BOOLEAN ;
                                  followinfo  : FollowDesc ;
                                  line        : CARDINAL ;
                                  description : Name ;
                               END ;

   DoProcedure    = PROCEDURE (ProductionDesc) ;

VAR
   f: FactorDesc ;
BEGIN
   f^.type := m2
END tinyvarient6.
