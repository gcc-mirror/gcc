// PR c++/121854
// { dg-do compile { target c++23 } }

struct S
{
    static void static_func(){}

    void func()
    {
        auto lambda = [](this auto)
        {
            static_func();
        };
    }
};
