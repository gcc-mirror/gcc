// PR c++/35297
// { dg-options "-std=c++11" }

template <class T=int, class... ARGS> 
struct test2 {};

int main()
{
        test2<> a;
        return 0;
}
