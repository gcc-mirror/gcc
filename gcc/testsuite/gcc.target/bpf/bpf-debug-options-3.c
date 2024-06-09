/* Using -g does not incorrectly force CO-RE enabled.  */
/* { dg-do compile } */
/* { dg-options "-g -dA -mno-co-re" }*/

struct A {
  int x;
  int y;
};

int
foo (struct A *a)
{
  return __builtin_preserve_access_index (a->x); /* { dg-error "BPF CO-RE is required" } */
}
