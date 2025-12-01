/* Check the vectorization of existing testsuite examples  */
/* Taken from pr54824.c  */
/* { dg-additional-options "-w" } */
/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

void __attribute__((noreturn)) bar(void)
{
}

void foo(int i, char *p, char *q)
{
  while (*p++) {
      if (i)
	p++;
      if (!*q++)
	bar();
  }
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
