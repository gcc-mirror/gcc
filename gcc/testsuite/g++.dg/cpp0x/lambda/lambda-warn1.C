// PR c++/41920
// { dg-options "-std=c++0x -Wall -Wextra" }

int foo(int i)
{
    auto bar = [=](){ return i; };
    return bar();
}
