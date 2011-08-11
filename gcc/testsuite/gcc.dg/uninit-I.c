/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int sys_msgctl (void)
{
  struct { int mode; } setbuf;
  return setbuf.mode;  /* { dg-warning "'setbuf\.mode' is used" "" } */
}
