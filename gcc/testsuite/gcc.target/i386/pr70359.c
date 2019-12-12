/* { dg-do compile } */
/* { dg-options "-O2" } */

char* inttostr(int i, char* buf, int len)
{
  unsigned int ui = (i > 0) ? i : -i;
  char *p = buf + len - 1;
  *p = '\0';
  do {
    *--p = '0' + (ui % 10);
  } while ((ui /= 10) != 0);
  if (i < 0) {
    *--p = '-';
  }
  return p;
}

/* In out-of-SSA we should have avoided splitting the latch edge of the
   loop by inserting copies.  */
/* { dg-final { scan-assembler-times "L\[0-9\]+:" 2 } } */
