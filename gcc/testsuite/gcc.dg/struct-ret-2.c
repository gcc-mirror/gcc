/* Simplified by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
   from bug report by Helmut Jarausch <jarausch@igpm.rwth-aachen.de>

   Copyright (C) 1999 Free Software Foundation  */

/* { dg-do compile } */
/* { dg-options "-O3 -w" } */

struct {
  unsigned i[4];
} foo() {}
 
void bar() {
  foo();           
}  
