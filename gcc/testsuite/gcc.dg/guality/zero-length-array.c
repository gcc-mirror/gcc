/* PR debug/86985 */
/* { dg-do run } */
/* { dg-options "-g" } */

struct {
  int foo;
  int bar[0];
} zla; /* Zero length array.  */

struct {
  int foo;
  int bar[];
} fam; /* Flexible array member.  */

int
main ()
{
  /* { dg-final { gdb-test . "type:zla" "struct { int foo; int bar[0]; }" } } */
  /* { dg-final { gdb-test . "type:fam" "struct { int foo; int bar[]; }" } } */
  return 0;
}
