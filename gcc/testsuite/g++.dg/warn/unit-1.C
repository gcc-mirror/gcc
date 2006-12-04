/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

struct a { int mode; };
int sys_msgctl (void)
{
  struct a setbuf;  /* { dg-warning "'setbuf\.a::mode' is used" } */
  return setbuf.mode;
}

