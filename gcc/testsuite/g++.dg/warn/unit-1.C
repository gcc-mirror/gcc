/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

struct a { int mode; };
int sys_msgctl (void)
{
  struct a setbuf;
  return setbuf.mode;  /* { dg-warning "'setbuf.a::mode' is used" } */
}

