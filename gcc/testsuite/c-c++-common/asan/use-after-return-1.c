/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_stack_use_after_return=1" } */
/* { dg-shouldfail "asan" } */

#include <stdio.h>
#include <pthread.h>

#ifndef kSize
# define kSize 1
#endif

#ifndef UseThread
# define UseThread 0
#endif

__attribute__((noinline))
char *Ident(char *x) {
  fprintf(stderr, "1: %p\n", x);
  return x;
}

__attribute__((noinline))
char *Func1() {
  char local[kSize];
  return Ident(local);
}

__attribute__((noinline))
void Func2(char *x) {
  fprintf(stderr, "2: %p\n", x);
  *x = 1;
}

void *Thread(void *unused)  {
  Func2(Func1());
  return NULL;
}

int main(int argc, char **argv) {
#if UseThread
  pthread_t t;
  pthread_create(&t, 0, Thread, 0);
  pthread_join(t, 0);
#else
  Func2(Func1());
#endif
  return 0;
}

/* { dg-output "WRITE of size 1 at .* thread T0.*" } */
/* { dg-output "    #0.*Func2.*use-after-return-1.c:31.*" } */
/* { dg-output "is located in stack of thread T0 at offset.*" } */
/* { dg-output "\'local\' <== Memory access at offset 32 is inside this variable" } */
