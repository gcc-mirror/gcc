// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

void impure() {}    // impure

auto fb1(T)() pure
{
    int x;
    struct A(S)
    {
        void fc(T2)()
        {
            x = 1;      // accessing pure function context is just ok
            impure();   // impure function call makes fc as impure
        }
        this(S a) {}
    }
    return A!int();
}
auto fb2(T)() pure
{
    int x;
    struct A(S)
    {
        void fc(T2)()
        {
            impure();   // impure function call makes fc as impure
            x = 1;      // accessing pure function context is just ok
        }
        this(S a) {}
    }
    return A!int();
}
void test1()
{
    fb1!int().fc!int();
    fb2!int().fc!int();
}
