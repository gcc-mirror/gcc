/* Regression test for:
   http://code.google.com/p/address-sanitizer/issues/detail?id=37 */

/* { dg-do run { target { *-*-linux* } } } */
/* { dg-require-effective-target clone } */
/* { dg-options "-D_GNU_SOURCE" } */

#include <stdio.h>
#include <stdlib.h>
#include <sched.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int Child(void *arg) {
  char x[32] = {0};  /* Stack gets poisoned. */
  printf("Child:  %p\n", x);
  _exit(1);  /* NoReturn, stack will remain unpoisoned unless we do something. */
}

volatile int zero = 0;

int main(int argc, char **argv) {
  int i;
  const int kStackSize = 1 << 20;
  char child_stack[kStackSize + 1];
  char *sp = child_stack + kStackSize;  /* Stack grows down. */
  printf("Parent: %p\n", sp);
  pid_t clone_pid = clone(Child, sp, CLONE_FILES | CLONE_VM, NULL, 0, 0, 0);
  int status;
  pid_t wait_result = waitpid(clone_pid, &status, __WCLONE);
  if (wait_result < 0) {
    perror("waitpid");
    return 1;
  }
  if (wait_result == clone_pid && WIFEXITED(status)) {
    /* Make sure the child stack was indeed unpoisoned. */
    for (i = 0; i < kStackSize; i++)
      child_stack[i] = i;
    int ret = child_stack[zero];
    return ret;
  }
  return 1;
}
