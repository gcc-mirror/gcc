// Test for range-based for loop when the declarator declares
// a new type

// { dg-do compile }
// { dg-options "-std=c++0x" }

#include <initializer_list>

void test()
{
    for (struct S { } *x : { (S*)0, (S*)0 } )
        ;

    for (struct S { } x : { S(), S() } )
        ;
}
