/* This test case would get an unresolved symbol during link
   because stabs referred to an optimized-away literal pool
   entry.  */

/* { dg-do run } */
/* { dg-options "-O2 -fno-omit-frame-pointer -gstabs" } */

int main (void)
{
  static char buf[4096];
  char *p;

  do
    {
      p = buf;
      asm volatile ("" : : : "memory", "0", "1", "2", "3", "4", "5", "6",
				       "7", "8", "9", "10", "12");
    }
  while (*p);

  return 0;
}

