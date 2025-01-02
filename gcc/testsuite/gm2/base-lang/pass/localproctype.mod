IMPLEMENTATION MODULE localproctype ;  (*!m2pim*)

TYPE
   myproc = PROCEDURE (CARDINAL) ;


PROCEDURE foo (c: CARDINAL) ;
BEGIN
END foo ;


VAR
   p: myproc ;
BEGIN
   p := foo
END localproctype.
