/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=2:halt_on_error=0" } */
/* { dg-options "-fsanitize=address,pointer-compare" } */

/* FIXME: remove me after PR sanitizer/82501 is resolved.  */
/* { dg-additional-options "-fno-section-anchors" } */
/* { dg-additional-options "-msdata=none" { target { powerpc*-*-* } } } */
/* { dg-additional-options "-msmall-data-limit=0" { target { riscv*-*-* } } } */

volatile int v;

__attribute__((noipa)) void
foo (char *p, char *q)
{
  v = p > q;
}

char __attribute__((used)) global1[100] = {};
char __attribute__((used)) global2[100] = {};
char __attribute__((used)) smallest_global[5] = {};
char __attribute__((used)) small_global[7] = {};
char __attribute__((used)) little_global[10] = {};
char __attribute__((used)) medium_global[4000] = {};
char __attribute__((used)) large_global[5000] = {};
char __attribute__((used)) largest_global[6000] = {};

int
main ()
{
  /* Heap allocated memory.  */
  char *heap1 = (char *)__builtin_malloc (42);
  char *heap2 = (char *)__builtin_malloc (42);

  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, heap2);
  __builtin_free (heap1);
  __builtin_free (heap2);

  heap1 = (char *)__builtin_malloc (1024);
  __asm ("" : "+g" (heap1));
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, heap1 + 1025);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1 + 1024, heap1 + 1025);
  __builtin_free (heap1);

  heap1 = (char *)__builtin_malloc (4096);
  __asm ("" : "+g" (heap1));
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, heap1 + 4097);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, 0);

  /* Global variables.  */
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (&global1[0], &global2[10]);

  char *p = &small_global[0];
  __asm ("" : "+g" (p));
  foo (p, p); /* OK */
  foo (p, p + 7); /* OK */
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p, p + 8);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p - 1, p);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p, p - 1);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p - 1, p + 8);

  p = &large_global[0];
  __asm ("" : "+g" (p));
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p - 1, p);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p, p - 1);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p, &global1[0]);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p, &small_global[0]);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (p, 0);

  /* Stack variables.  */
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  char stack1, stack2;
  foo (&stack1, &stack2);

  /* Mixtures.  */
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, &stack1);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, &global1[0]);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (&stack1, &global1[0]);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair" } */
  foo (&stack1, 0);
  __builtin_free (heap1);

  return 0;
}
