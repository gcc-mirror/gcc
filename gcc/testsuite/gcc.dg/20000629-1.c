/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

void foo(register char *p)
{
  char c, *q, *sp;
  while (1) {
    *p++=0;
    sp=p+1;
    c=*sp;
    *p++=0;
  }
}
