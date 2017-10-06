/* { dg-require-effective-target lp64 } */
/* { dg-options "-O -fsanitize=pointer-overflow" } */
/* { dg-skip-if "" { *-*-* } "-flto" } */

#define SMAX   __PTRDIFF_MAX__

void foo(void)
{
  char *p;
  char *p2;
  char b[1];
  char c[1];

  p = b + SMAX; /* pointer overflow check is needed */
  p = b;
  p++;
  p2 = p + 1000;
  p2 = p + 999;

  p = b + SMAX;
  p2 = p + 1; /* pointer overflow check is needed */

  p = b;
  p--; /* pointer overflow check is needed */
  p2 = p + 1;
  p2 = p + 2;

  p = b - SMAX; /* pointer overflow check is needed */
  p2 = p + (SMAX - 2); /* b - 2: pointer overflow check is needed */
  p2 = p + (SMAX - 1); /* b - 1: pointer overflow check is needed */
  p2 = p + SMAX; /* b: pointer overflow check is needed */
  p2++; /* b + 1 */

  p = c;
  p++; /* c + 1 */
  p = c - SMAX; /* pointer overflow check is needed */
  p2 = p + SMAX; /* c: pointer overflow check is needed */
  p2++; /* c + 1 */
}

void bar(char *ptr)
{
  char *p = ptr - 1000; /* pointer overflow check is needed */
  p = ptr + 1000; /* pointer overflow check is needed */
  p -= 2000; /* pointer overflow check is needed */
}

void baz(char *ptr)
{
  char **p = &ptr;
  char **p2 = p + 20; /* pointer overflow check is needed */
  p2--;
}

void positive_and_positive (char *ptr)
{
  char **p = &ptr;
  char **p2 = p + 100; /* pointer overflow check is needed */
  p2 = p + 10;
  p += 50; 
}

void negative_to_positive (char *ptr)
{
  char **p = &ptr;
  char **p2 = p + 20; /* pointer overflow check is needed */
  p2 = p - 10; /* pointer overflow check is needed */
  p2 += 15;
}

void negative_to_negative (char *ptr)
{
  char **p = &ptr;
  char **p2 = p - 20; /* pointer overflow check is needed */
  p2 = p - 20;
  p2 += 5;
}


/* { dg-final { scan-assembler-times "call\\s+__ubsan_handle_pointer_overflow" 17 } } */
