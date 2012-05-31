/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 200

void foo (unsigned char *__restrict__ pInput, unsigned char *__restrict__ pOutput)
{
  unsigned char i, a, b, c;

  for (i = 0; i < N / 3; i++)
    {
       a = *pInput++;
       b = *pInput++;
       c = *pInput++;

       *pOutput++ = a + b + c + 3;
       *pOutput++ = a + b + c + 12;
       *pOutput++ = a + b + c + 1;
    }
}

int main (int argc, const char* argv[])
{
  unsigned char input[N], output[N], i;
  unsigned char check_results[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i;
      output[i] = 0;
      __asm__ volatile ("");
    }

  for (i = 0; i < N / 3; i++)
    {
      check_results[3*i] = 9 * i + 6;
      check_results[3*i+1] = 9 * i + 15;
      check_results[3*i+2] = 9 * i + 4;
    }

  foo (input, output);

  for (i = 0; i < N - (N % 3); i++)
     if (output[i] != check_results[i])
       abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target { vect_perm_byte && vect_char_mult } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_perm_byte && {! vect_char_mult } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target vect_perm_byte } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

