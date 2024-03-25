/* { dg-do compile } */
/* { dg-options "-finline-stringops" } */

char buf[3];

int
f ()
{
  __builtin_memset (buf, 'v', 3);
}
