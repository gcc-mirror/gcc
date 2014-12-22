// PR c++/64333
// { dg-do compile { target c++14 } }
#include <initializer_list>

constexpr int max(std::initializer_list<int> ints)
{
        int ret = *(ints.begin());
        for (int i = 0; i < ints.size(); ++i) {
                if (*(ints.begin()+i) > ret) {
                        ret = *(ints.begin()+i);
                }
        }
        return ret;
}

int main()
{
        constexpr int z = max({7,6,5,4,3,2,1});
        constexpr int z2 = max({5,4,3,2,1});
        static_assert(z == 7, "");
        static_assert(z2 == 5, "");
}
