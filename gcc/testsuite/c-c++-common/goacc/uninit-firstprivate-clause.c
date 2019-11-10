/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

void
foo_parallel (void)
{
  int i;

#pragma acc parallel
  {
    i = 1;
  }
}

void
foo_serial (void)
{
  int i;

#pragma acc serial
  {
    i = 1;
  }
}


void
foo2_parallel (void)
{
  int i;
  /* { dg-note {'i' was declared here} {} { target *-*-* } .-1 } */

#pragma acc parallel firstprivate (i) /* { dg-warning "is used uninitialized" } */
  {
    i = 1;
  }
}

void
foo2_serial (void)
{
  int i;
  /* { dg-note {'i' was declared here} {} { target *-*-* } .-1 } */

#pragma acc serial firstprivate (i) /* { dg-warning "is used uninitialized" } */
  {
    i = 1;
  }
}
