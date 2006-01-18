/* { dg-do compile } */

void XAXIS ();
void YAXIS ();
void ZAXIS ();
void
a9 ()
{
#pragma omp parallel sections
  {
#pragma omp section
    XAXIS ();
#pragma omp section
    YAXIS ();
#pragma omp section
    ZAXIS ();
  }
}
