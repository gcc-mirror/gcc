// { dg-do compile }
// { dg-options "-fno-druntime -gdwarf-4 -dA -fno-merge-debug-strings" }
// { dg-final { scan-assembler-times "DIE\[^\n\r\]*DW_TAG_enumeration_type" 1 } }
// { dg-final { scan-assembler-times " DW_AT_enum_class" 1 } }
// { dg-final { scan-assembler-times "\"E..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"E1..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"C1..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"C2..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"C3..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"C4..\"\[^\n\]*DW_AT_name" 1 } }
// { dg-final { scan-assembler-times "\"S1..\"\[^\n\]*DW_AT_name" 1 } }

module expression;
extern (C++):
class C1
{
    bool bfn() { return true; }
}
class C2 : C1
{
    C4 cfn() { return null; }
}
class C3 : C2
{
    S1.E s;
}
class C4 : C3
{
    S1 s;
}
struct S1
{
    enum E : ubyte { E1 }
    E e;
    C3 c;
}
