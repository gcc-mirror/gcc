// PR debug/58150
// { dg-do compile }
// { dg-options "-std=c++11 -gdwarf-4 -dA -fno-merge-debug-strings" }
// { dg-final { scan-assembler-times "DIE\[^\n\r\]*DW_TAG_enumeration_type" 3 } }
// { dg-final { scan-assembler-times " DW_AT_enum_class" 3 } }
// { dg-final { scan-assembler-times " DW_AT_declaration" 1 } }
// { dg-final { scan-assembler-times "\"E1..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"E2..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"F1..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"F2..\"\[^\n\]*DW_AT_name" 1 } }

enum class E : int;
enum class F : int;
enum class G : int;
struct S { E s; };
struct T { G t; };
enum class E : int
{
  E1, E2
};
enum class F : int
{
  F1, F2
};

bool
foo (E e, F f, G g)
{
  return e == E::E1 && f == F::F1 && (int) g == 0;
}
