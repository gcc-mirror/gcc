// PR c++/114632
// { dg-do compile { target c++23 } }

struct S {};

auto lambda = [](this auto& self, const int x) /* -> void */ {};

int main()
{
    void (*func)(S&, int) = lambda; // { dg-error "" }
    return 0;
}
