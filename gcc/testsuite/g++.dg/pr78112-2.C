/* { dg-do compile } */
/* { dg-skip-if "No dwarf debug support" { hppa*-*-hpux* } } */
/* { dg-options "-g -dA -gdwarf-4 -std=gnu++11" } */
/* { dg-options "-g -dA -std=gnu++11 -gdwarf-4" } */
/* { dg-final { scan-assembler-times DW_AT_object_pointer 12 } } */

void run (int *int_p, void(*func)(int *)) { func (int_p); }
namespace foo {
   struct Foo {
      int a;
      Foo() { run (&a, [](int *int_p) { *int_p = 0; }); }
   };
}
int main (void) { foo::Foo f; }
