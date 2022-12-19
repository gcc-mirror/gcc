/* { dg-do assemble } */
/* { dg-options {-save-temps -O0 -msoft-stack} } */

extern void *__nvptx_stacks[32] __attribute__((shared,nocommon));

void *f()
{
  /* Implicit '__nvptx_stacks' usage for frame; per 'init_softstack_frame':
     { dg-final { scan-assembler-times {mov\.u64 %fstmp2, __nvptx_stacks;} 1 } }
  */
  void *stack_array[123];
  /* Explicit '__nvptx_stacks' usage.  */ 
  stack_array[5] = __nvptx_stacks[0];
  return stack_array[5];
}

/* Of the implicit (via 'need_softstack_decl') and explicit declarations of
   '__nvptx_stacks', only one is emitted:
   { dg-final { scan-assembler-times {(?n)\.extern .* __nvptx_stacks\[32\];} 1 } }
*/
