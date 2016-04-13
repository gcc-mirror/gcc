/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

void
foo (void)
{
  int i;

#pragma acc kernels
  {
    i = 1;
  }

}

void
foo2 (void)
{
  int i;

#pragma acc kernels copy (i)
  {
    i = 1;
  }

}

void
foo3 (void)
{
  int i;

#pragma acc kernels copyin(i)
  {
    i = 1;
  }

}
