/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O3 -ftree-parallelize-loops=2" } */

int foo ()
{
  int i, sum = 0, data[1024];

  for(i = 0; i<1024; i++)
    sum += data[i];

  return sum;
}

