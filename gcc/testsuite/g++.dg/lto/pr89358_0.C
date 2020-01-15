/* { dg-do link } */
/* { dg-options "-std=c++17"  } */
#include <map>

extern void test();

int main()
{
        std::map<int, int> m;
        test();
}
