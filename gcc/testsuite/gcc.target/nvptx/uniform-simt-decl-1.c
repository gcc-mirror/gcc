/* { dg-do compile } */
/* { dg-options {-save-temps -O0 -muniform-simt} } */

extern unsigned __nvptx_uni[32] __attribute__((shared,nocommon));

enum memmodel
{
  MEMMODEL_RELAXED = 0,
};

int a = 0;

int f (void)
{
  /* Explicit '__nvptx_uni' usage.  */
  __builtin_printf("%u\n", __nvptx_uni[0]);

  /* Implicit '__nvptx_uni' usage; per 'nvptx_init_unisimt_predicate':
     { dg-final { scan-assembler-times {mov\.u64 %r[0-9]+, __nvptx_uni;} 1 } }
  */
  int expected = 1;
  return __atomic_compare_exchange_n (&a, &expected, 0, 0, MEMMODEL_RELAXED,
				      MEMMODEL_RELAXED);
}

/* The implicit (via 'need_unisimt_decl') and explicit declarations of
   '__nvptx_uni' are both emitted:
   { dg-final { scan-assembler-times {(?n)\.extern .* __nvptx_uni\[32\];} 2 } }
*/
