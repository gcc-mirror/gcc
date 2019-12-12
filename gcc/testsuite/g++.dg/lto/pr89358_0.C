/* { dg-lto-do link } */
/* { dg-lto-options "-std=c++17"  } */
#include <map>

extern void test();

int main()
{
        std::map<int, int> m;
        test();
}
