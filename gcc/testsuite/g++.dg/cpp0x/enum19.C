// We shouldn't give an ABI warning about promotion in switch.
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=5 -Wabi" }

enum class Foo { X };
void test(Foo val)
{
    switch(val)
    {
    case Foo::X:
        break;
    }
};
