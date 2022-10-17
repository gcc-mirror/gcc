/* { dg-do link } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-fno-PIC" { target ia32 } } */
/* { dg-additional-sources "pr102892-2.c" } */

static long b[2][1] = {0};

extern void bar(void);
extern void foo(void);

int
main ()
{
  long c = 0;
  for (long a = 0; a < 1; ++a)
    for (; c <= 1; c++) {
      bar();
      if (1 == b[c][0])
	foo();
    }
  return 0;
}
