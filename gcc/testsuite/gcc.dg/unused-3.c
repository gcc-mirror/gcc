/* Copyright (C) 2000  Free Software Foundation.  */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

typedef short unused_type __attribute__ ((unused));
main ()
{
  short x;  /* { dg-warning "unused variable" "unused variable warning" } */
  unused_type y;
}
