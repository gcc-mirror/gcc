/* PR c/68966 - atomic_fetch_* on atomic_bool not diagnosed
   Test to verify that calls to __atomic_fetch_op funcions with a _Bool
   argument are rejected.  This is necessary because GCC expects that
   all initialized _Bool objects have a specific representation and
   allowing atomic operations to change it would break the invariant.  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors -std=c11" } */


void test_atomic_bool (_Atomic _Bool *a)
{
  enum { SEQ_CST = __ATOMIC_SEQ_CST };
  
  __atomic_fetch_add (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_fetch_add." } */
  __atomic_fetch_sub (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_fetch_sub." } */
  __atomic_fetch_and (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_fetch_and." } */
  __atomic_fetch_xor (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_fetch_xor." } */
  __atomic_fetch_or (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_fetch_or." } */
  __atomic_fetch_nand (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_fetch_nand." } */

  __atomic_add_fetch (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_add_fetch." } */
  __atomic_sub_fetch (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_sub_fetch." } */
  __atomic_and_fetch (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_and_fetch." } */
  __atomic_xor_fetch (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_xor_fetch." } */
  __atomic_or_fetch (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_or_fetch." } */
  __atomic_nand_fetch (a, 1, SEQ_CST);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__atomic_nand_fetch." } */

  /* The following are valid and must be accepted.  */
  _Bool val = 0, ret = 0;
  __atomic_exchange (a, &val, &ret, SEQ_CST);
  __atomic_exchange_n (a, val, SEQ_CST);
  __atomic_compare_exchange (a, &val, &ret, !1, SEQ_CST, SEQ_CST);
  __atomic_compare_exchange_n (a, &val, ret, !1, SEQ_CST, SEQ_CST);
  __atomic_test_and_set (a, SEQ_CST);
  __atomic_clear (a, SEQ_CST);
}

void test_bool (_Bool *b)
{
  enum { SEQ_CST = __ATOMIC_SEQ_CST };
  
  __atomic_fetch_add (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_fetch_add." } */
  __atomic_fetch_sub (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_fetch_sub." } */
  __atomic_fetch_and (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_fetch_and." } */
  __atomic_fetch_xor (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_fetch_xor." } */
  __atomic_fetch_or (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_fetch_or." } */
  __atomic_fetch_nand (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_fetch_nand." } */

  __atomic_add_fetch (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_add_fetch." } */
  __atomic_sub_fetch (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_sub_fetch." } */
  __atomic_and_fetch (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_and_fetch." } */
  __atomic_xor_fetch (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_xor_fetch." } */
  __atomic_or_fetch (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_or_fetch." } */
  __atomic_nand_fetch (b, 1, SEQ_CST);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__atomic_nand_fetch." } */

  /* The following are valid and must be accepted.  */
  _Bool val = 0, ret = 0;
  __atomic_exchange (b, &val, &ret, SEQ_CST);
  __atomic_exchange_n (b, val, SEQ_CST);
  __atomic_compare_exchange (b, &val, &ret, !1, SEQ_CST, SEQ_CST);
  __atomic_compare_exchange_n (b, &val, ret, !1, SEQ_CST, SEQ_CST);
  __atomic_test_and_set (b, SEQ_CST);
  __atomic_clear (b, SEQ_CST);
}
