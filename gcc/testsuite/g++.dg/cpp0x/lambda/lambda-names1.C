// PR c++/56710
// { dg-options "-std=c++11 -Wall" }

int main()
{
    int t = 0;
    return [&]() -> int {int __t; __t = t; return __t; }();
    return [&t]() -> int {int __t; __t = t; return __t; }();
}
