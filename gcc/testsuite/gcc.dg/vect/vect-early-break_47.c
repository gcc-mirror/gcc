/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_float } */

void abort ();

float results1[16] = {192.00,240.00,288.00,336.00,384.00,432.00,480.00,528.00,0.00};
float results2[16] = {0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,54.00,120.00,198.00,288.00,390.00,504.00,630.00};
float a[16] = {0};
float e[16] = {0};
float b[16] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
int main1 ()
{
  int i;
  for (i=0; i<16; i++)
    {
      if (a[i] != results1[i] || e[i] != results2[i])
        abort();
    }

  if (a[i+3] != b[i-1])
    abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
