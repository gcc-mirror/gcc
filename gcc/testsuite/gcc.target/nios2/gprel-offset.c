/* { dg-do compile } */
/* { dg-options "-O2" } */

static struct s {
  int x;
  char y;
} s;

void set (char c)
{
  s.y = c;
}


char get (void)
{
  return s.y;
}

/* { dg-final { scan-assembler-times "%gprel\\(s\\+4\\)\\(gp\\)" 2 } } */
