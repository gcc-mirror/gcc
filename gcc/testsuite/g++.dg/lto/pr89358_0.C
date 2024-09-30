/* { dg-lto-do link } */
/* { dg-lto-options "-std=c++17"  } */
/* { dg-skip-if "requires hosted libstdc++ for map" { ! hostedlib } } */
#include <map>

extern void test();

int main()
{
        std::map<int, int> m;
        test();
}
