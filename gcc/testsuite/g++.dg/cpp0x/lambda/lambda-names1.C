// PR c++/56710
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

int main()
{
    int t = 0;
    return [&]() -> int {int __t; __t = t; return __t; }();
    return [&t]() -> int {int __t; __t = t; return __t; }();
}
