/* Check that ASan plays well with easy cases of makecontext/swapcontext. */

/* { dg-do run { target swapcontext } } */

#include <stdio.h>
#include <ucontext.h>
#include <unistd.h>

ucontext_t orig_context;
ucontext_t child_context;

void Child(int mode) {
  char x[32] = {0};  /* Stack gets poisoned. */
  printf("Child: %p\n", x);
  /* (a) Do nothing, just return to parent function.
     (b) Jump into the original function. Stack remains poisoned unless we do
         something. */
  if (mode == 1) {
    if (swapcontext(&child_context, &orig_context) < 0) {
      perror("swapcontext");
      _exit(0);
    }
  }
}

int Run(int arg, int mode) {
  int i;
  const int kStackSize = 1 << 20;
  char child_stack[kStackSize + 1];
  printf("Child stack: %p\n", child_stack);
  /* Setup child context. */
  getcontext(&child_context);
  child_context.uc_stack.ss_sp = child_stack;
  child_context.uc_stack.ss_size = kStackSize / 2;
  if (mode == 0) {
    child_context.uc_link = &orig_context;
  }
  makecontext(&child_context, (void (*)())Child, 1, mode);
  if (swapcontext(&orig_context, &child_context) < 0) {
    perror("swapcontext");
    return 0;
  }
  /* Touch childs's stack to make sure it's unpoisoned. */
  for (i = 0; i < kStackSize; i++) {
    child_stack[i] = i;
  }
  return child_stack[arg];
}

volatile int zero = 0;

int main(int argc, char **argv) {
  int ret = 0;
  ret += Run(zero, 0);
  fprintf(stderr, "Test1 passed\n");
  ret += Run(zero, 1);
  fprintf(stderr, "Test2 passed\n");
  return ret;
}

/* { dg-output "WARNING: ASan doesn't fully support makecontext/swapcontext.*" } */
/* { dg-output "Test1 passed.*" } */
/* { dg-output "Test2 passed.*" } */
