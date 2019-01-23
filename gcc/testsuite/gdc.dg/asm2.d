// { dg-do compile }
module asm2;

void test()
{
    asm const shared { }    // { dg-error "const/immutable/shared/inout attributes are not allowed on asm blocks" }
}

