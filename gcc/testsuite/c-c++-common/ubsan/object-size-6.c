/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=object-size" } */

char
foo (void *v)
{
  return *(char *) v;
}
