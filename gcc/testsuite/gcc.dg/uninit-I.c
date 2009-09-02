/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int sys_msgctl (void)
{
  struct { int mode; } setbuf;  /* { dg-warning "'setbuf\.mode' is used" "" { xfail *-*-* } } */
  return setbuf.mode;
}
