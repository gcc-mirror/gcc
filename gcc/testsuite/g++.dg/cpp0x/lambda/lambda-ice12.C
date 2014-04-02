// PR c++/54250
// { dg-do compile { target c++11 } }

struct T
{
    int a;
    int foo()
    {
        return [&]()->int {
            return [&](decltype(/*this->*/a) _)->int {
                return 1;
            }(a);
        }();
    }
};
