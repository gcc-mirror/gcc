// https://issues.dlang.org/show_bug.cgi?id=20593
// { dg-do compile }
// { dg-options "-Wall -Wdeprecated -Werror" }
module asm5;

void test(int a)
{
    asm
    {
        "cpuid" : : "a" a;  // { dg-error "'a' must be surrounded by parentheses" }
    }
}
