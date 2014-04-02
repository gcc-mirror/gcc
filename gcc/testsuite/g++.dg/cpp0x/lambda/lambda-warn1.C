// PR c++/41920
// { dg-do compile { target c++11 } }
// { dg-options "-Wall -Wextra" }

int foo(int i)
{
    auto bar = [=](){ return i; };
    return bar();
}
