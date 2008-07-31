/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

int sys_msgctl (void)
{
  struct { int mode; } setbuf;  /* { dg-warning "'setbuf\.mode' is used" {} { xfail *-*-* } } */
  return setbuf.mode;
}
