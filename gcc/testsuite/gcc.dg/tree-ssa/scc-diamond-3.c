/* { dg-do compile } */
/* { dg-options "-O2 -fstrict-aliasing -fdump-tree-phiopt-details" } */

/* PR tree-optimization/125557.  Alias-type merging in factor_out_conditional_load.

   Same data-dependent load-address recurrence as scc-diamond-1.c, but the two
   arms read the next tag through pointers with *different* TBAA alias types: a
   plain "const unsigned char" load in one arm and a may_alias load in the other.
   The two MEM_REFs have the same value type (unsigned char) but different
   operand-1 (alias-ptr) types.  Rather than refusing to common loads with
   mismatched alias types, factor_out_conditional_load merges them the way
   get_alias_type_for_stmts does: since the types are incompatible the combined
   load is given ptr_type_node (the alias-everything type) and the dependence
   clique/base are dropped, so it conservatively conflicts with any store either
   original arm could.  The two conditional loads are therefore commoned into a
   single reg-offset load and the diamond is if-converted.  */

#include <stddef.h>
#include <stdint.h>

typedef uint8_t alias_u8 __attribute__((may_alias));

const uint8_t *
advance (const uint8_t *ip, size_t tag, const uint8_t *end)
{
  while (ip < end)
    {
      size_t type = tag & 3;
      if (type == 0)
	{
	  size_t nlt = (tag >> 2) + 1;
	  /* Plain alias type: operand-1 type is "const unsigned char *".  */
	  tag = ip[nlt];
	  ip += nlt + 1;
	}
      else
	{
	  /* may_alias: operand-1 type is the alias-everything pointer.  Same
	     value type (unsigned char), different operand-1 type.  */
	  tag = *(const alias_u8 *) (ip + type);
	  ip += type + 1;
	}
    }
  return ip;
}

/* The mismatched alias types are merged to the alias-everything type, so the
   loads are commoned and the diamond is if-converted.  */
/* { dg-final { scan-tree-dump-times "changed to factor out load from" 1 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 1 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 2 "phiopt1" } } */
