/* Copyright (C) 2000  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do preprocess } */

#line 1
#line 0 
#line 2
#line 32768 

/* { dg-error "out of range" "line # too low" { target *-*-* } 1 } */
/* { dg-error "out of range" "line # too high" { target *-*-* } 2 } */
