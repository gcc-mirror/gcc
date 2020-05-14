/* Make sure a stack probe is emitted also for the remaining bytes
   after the loop probing the large chunk.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=z9-ec -fstack-clash-protection" } */

void large_stack() {
  volatile int stack[8000];
  int i;
  for (i = 0; i < sizeof(stack) / sizeof(int); ++i)
    stack[i] = i;
}

/* We use a compare for the stack probe.  There needs to be one inside
   a loop and another for the remaining bytes.  */
/* { dg-final { scan-assembler-times "cg\t" 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times "c\t" 2 { target { ! lp64 } } } } */
