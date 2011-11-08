/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fopenmp" } */

__attribute__ ((transaction_pure))
unsigned long rdtsc();

typedef struct ENTER_EXIT_TIMES
{
  unsigned long enter;
} times_t;

void ParClassify()
{
  void * Parent;
#pragma omp parallel private(Parent)
  {
    times_t inside;
    __transaction_atomic {
       inside.enter = rdtsc();
    }
  }
}
