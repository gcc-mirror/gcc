/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-require-effective-target fopenacc } */
/* { dg-options "-O2 -mx32 -fopenacc" } */

typedef struct {
  int a;
  int b;
  int c;
} mystruct;
int main_j;
int
main()
{
  mystruct *m = (mystruct *)__builtin_malloc (2*sizeof (mystruct)), *mref = m;
#pragma acc enter data copyin(m[1])
  for (int i; i < 9; i++) {
#pragma acc parallel
    for (; main_j;)
      ;
#pragma acc parallel loop copy(mref->b, m->c)
    for (main_j = 0; main_j < 4; main_j++)
      ;
  }
#pragma acc data copyout(m[ : 1])
  __builtin_free(m);
}
