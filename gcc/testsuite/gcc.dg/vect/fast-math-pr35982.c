/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_int } */

struct mem 
{
  float avg;
  int len;
};

float method2_int16 (struct mem *mem)
{
  int i;
  float avg;

  for (i = 0; i < 100; ++i)
     avg += mem[i].avg * (float) mem[i].len;

  return avg;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_extract_even_odd_wide  } } } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { xfail vect_extract_even_odd_wide  } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

