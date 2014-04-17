/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

int sys_msgctl (void)
{
  struct { int mode; } setbuf;
  return setbuf.mode; /* { dg-warning "'setbuf\.mode' is used uninitialized in this function" } */
}
