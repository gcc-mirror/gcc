// PR c++/114632
// { dg-do compile { target c++23 } }

struct S {};

auto lambda = [](auto, const int x) static /* -> void */ {};

int main()
{
    void (*func)(int, int) = lambda;
    return 0;
}
