
// { dg-do compile }

extern void bar (void);

int main (void)
{
  #pragma omp parallel sections nowait /* { dg-error "'nowait'" } */
    {
    #pragma omp section
	{ bar(); }
    #pragma omp section
	{ bar(); }
    }
}
