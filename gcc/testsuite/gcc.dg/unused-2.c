/* Copyright (C) 2000  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do compile } */
/* { dg-options "-O2 -finline-functions -Wreturn-type" } */

static void
foo ()
{
  skip_it: ; /* { dg-warning "defined but not used" "unused label warning" } */
}

void
bar ()
{
  foo ();
}
