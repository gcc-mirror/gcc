/* Copyright (C) 2000, 2003  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do preprocess } */

/* The line number below must be just a few lines greater than the
   actual line number. */
#line 10 "baz"
wibble

/* { dg-final { scan-file line1.i baz } } */
