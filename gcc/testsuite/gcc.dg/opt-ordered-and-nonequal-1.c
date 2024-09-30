/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1-details" } */
/* Make this work for default 'LOGICAL_OP_NON_SHORT_CIRCUIT == false' configurations:
   { dg-additional-options "--param logical-op-non-short-circuit=1" } */

int is_ordered_and_nonequal_sh_1 (float a, float b)
{
  return !__builtin_isunordered (a, b) && (a != b);
}

int is_ordered_and_nonequal_sh_2 (float a, float b)
{
  return !__builtin_isunordered (a, b) && (b != a);
}

int is_ordered_and_nonequal_sh_3 (float a, float b)
{
  return (b != a) && !__builtin_isunordered (a, b);
}

int is_ordered_and_nonequal_sh_4 (float a, float b)
{
  return !__builtin_isunordered (a, b) && !(a == b);
}

int is_ordered_and_nonequal_sh_5 (float a, float b)
{
  return !__builtin_isunordered (a, b) && !(b == a);
}

int is_ordered_and_nonequal_sh_6 (float a, float b)
{
  return !(b == a) && !__builtin_isunordered (a, b);
}

int is_ordered_or_nonequal_sh_7 (float a, float b)
{
  return !(__builtin_isunordered (a, b) || (a == b));
}

int is_ordered_or_nonequal_sh_8 (float a, float b)
{
  return !(__builtin_isunordered (a, b) || (b == a));
}

int is_ordered_or_nonequal_sh_9 (float a, float b)
{
  return !((a == b) || __builtin_isunordered (b, a));
}

/* { dg-final { scan-tree-dump-times "gimple_simplified to\[^\n\r]*<>" 9 "forwprop1" } } */
