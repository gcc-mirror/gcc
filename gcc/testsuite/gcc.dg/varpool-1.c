/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "unnecesary_static_initialized_variable" } } */

static int unnecesary_static_initialized_variable;
static int *unnecesary_static_initialized_variable2 =
  &unnecesary_static_initialized_variable;
static inline
simplify_after_inline (int param1, int *param2)
{
  if (unnecesary_static_initialized_variable != param1)
    return unnecesary_static_initialized_variable;
  if (unnecesary_static_initialized_variable2 != param2)
    return unnecesary_static_initialized_variable;
}

main ()
{
  return simplify_after_inline (unnecesary_static_initialized_variable,
				unnecesary_static_initialized_variable2) +
    simplify_after_inline (unnecesary_static_initialized_variable,
			   unnecesary_static_initialized_variable2);
}
