/* PR c/68966 - atomic_fetch_* on atomic_bool not diagnosed
   Test to verify that calls to __sync_fetch_op funcions with a _Bool
   argument are rejected.  This is necessary because GCC expects that
   all initialized _Bool objects have a specific representation and
   allowing atomic operations to change it would break the invariant.  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors -std=c11" } */


void test_sync_atomic_bool (_Atomic _Bool *a)
{
  __sync_fetch_and_add (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_add." } */
  __sync_fetch_and_sub (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_sub." } */
  __sync_fetch_and_and (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_and." } */
  __sync_fetch_and_xor (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_xor." } */
  __sync_fetch_and_or (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_or." } */
  __sync_fetch_and_nand (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_nand." } */
  
  __sync_add_and_fetch (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_add_and_fetch." } */
  __sync_sub_and_fetch (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_sub_and_fetch." } */
  __sync_and_and_fetch (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_and_and_fetch." } */
  __sync_xor_and_fetch (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_xor_and_fetch." } */
  __sync_or_and_fetch (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_or_and_fetch." } */
  __sync_nand_and_fetch (a, 1);   /* { dg-error "operand type ._Atomic _Bool \\*. is incompatible with argument 1 of .__sync_nand_and_fetch." } */

  /* The following are valid and must be accepted.  */
  __sync_bool_compare_and_swap (a, 0, 1);
  __sync_val_compare_and_swap (a, 0, 1);
  __sync_lock_test_and_set (a, 1);
  __sync_lock_release (a);
}

void test_sync_bool (_Bool *b)
{
  __sync_fetch_and_add (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_add." } */
  __sync_fetch_and_sub (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_sub." } */
  __sync_fetch_and_and (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_and." } */
  __sync_fetch_and_xor (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_xor." } */
  __sync_fetch_and_or (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_or." } */
  __sync_fetch_and_nand (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_fetch_and_nand." } */
  
  __sync_add_and_fetch (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_add_and_fetch." } */
  __sync_sub_and_fetch (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_sub_and_fetch." } */
  __sync_and_and_fetch (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_and_and_fetch." } */
  __sync_xor_and_fetch (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_xor_and_fetch." } */
  __sync_or_and_fetch (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_or_and_fetch." } */
  __sync_nand_and_fetch (b, 1);   /* { dg-error "operand type ._Bool \\*. is incompatible with argument 1 of .__sync_nand_and_fetch." } */

  /* The following are valid and must be accepted.  */
  __sync_bool_compare_and_swap (b, 0, 1);
  __sync_val_compare_and_swap (b, 0, 1);
  __sync_lock_test_and_set (b, 1);
  __sync_lock_release (b);
}
