/* Copyright (C) 2000  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do preprocess } */

/* The line number below must be just a few lines greater than the
   actual line number. */
#line 10 "baz"
wibble

/*
  { dg-final { if \{ [grep line1.i baz] != "" \} \{ } }
  { dg-final {   pass "line1.i: #line directive optimization" } }
  { dg-final { \} else \{ } }
  { dg-final {   fail "line1.i: #line directive optimization" } }
  { dg-final { \} } }
*/
