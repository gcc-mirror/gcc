// PR c++/108242
// { dg-do compile { target c++14 } }

template<int F>
void my_fun()
{
    [&](auto) {
        static constexpr char const* fun_name = __func__;
        struct t
        {
            t() { fun_name; };
        } t1;
    }(12);
}

int main() {
    my_fun<1>();
}
