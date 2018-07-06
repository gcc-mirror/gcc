/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=2 halt_on_error=0" } */
/* { dg-options "-fsanitize=address,pointer-subtract" } */

volatile __PTRDIFF_TYPE__ v;

__attribute__((noipa)) void
foo (char *p, char *q)
{
  v = p - q;
}

char global1[100] = {}, global2[100] = {};

int
main ()
{
  /* Heap allocated memory.  */
  char *heap1 = (char *)__builtin_malloc (42);
  char *heap2 = (char *)__builtin_malloc (42);

  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, heap2);

  /* Global variables.  */
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (&global1[0], &global2[10]);

  /* Stack variables.  */
  char stack1, stack2;
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (&stack1, &stack2);

  /* Mixtures.  */
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, &stack1);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair.*" } */
  foo (heap1, &global1[0]);
  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair" } */
  foo (&stack1, &global1[0]);

  __builtin_free (heap1);
  __builtin_free (heap2);  
  return 0;
}
