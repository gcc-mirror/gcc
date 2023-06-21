/* { dg-do compile } */
/* { dg-options "-mno-inline-atomics" } */
/* { dg-message "note: '__sync_fetch_and_nand' changed semantics in GCC 4.4" "fetch_and_nand" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "\tcall\t__sync_fetch_and_add_1" } } */
/* { dg-final { scan-assembler "\tcall\t__sync_fetch_and_nand_1" } } */
/* { dg-final { scan-assembler "\tcall\t__sync_bool_compare_and_swap_1" } } */

char foo;
char bar;
char baz;

int
main ()
{
  __sync_fetch_and_add(&foo, 1);
  __sync_fetch_and_nand(&bar, 1);
  __sync_bool_compare_and_swap (&baz, 1, 2);
}
