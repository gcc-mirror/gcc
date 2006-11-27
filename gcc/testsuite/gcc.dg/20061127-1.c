/* { dg-do compile { target lp64 } } */
/* { dg-options "-O1 -fPIC" } */

/* PR target/29319 */

extern void abort(void);
static char l_info[100];

void
bug1 (unsigned long tag)
{
  char *info = l_info;
  info[tag - 0x100000000 + 1] = 1;
}

void
bug2 (unsigned long tag)
{
  char *info = l_info;
  info[tag - 0x700000000 + 2] = 2;
}

void
bug3 (unsigned long tag)
{
  char *info = l_info;
  info[tag - 0x100000000 + 1] = 3;
}
