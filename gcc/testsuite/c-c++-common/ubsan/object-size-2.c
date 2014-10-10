/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined" } */

void
foo (unsigned long ul)
{
  unsigned int u;
  u = *(unsigned long *) ul;
}
