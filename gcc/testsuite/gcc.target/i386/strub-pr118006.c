/* { dg-do compile } */
/* { dg-require-effective-target strub } */
/* { dg-options "-fstrub=all -O2 -mno-accumulate-outgoing-args" } */

__attribute__((noipa))
long _raw_syscall(void *, long, long) {
  __builtin_abort();
}

static int privileged_traced_syscall() {
  return _raw_syscall(0, 0, 0);
}

void privileged_traced_raise() {
  privileged_traced_syscall();
  __builtin_unreachable ();
}
