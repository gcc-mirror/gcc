// { dg-do link }
// { dg-options "-gsplit-dwarf -fdebug-types-section" }

inline void foo() {}

struct Y
{
    template<void (*func)()>
    static void bar() {}
};

int main()
{
    Y::bar<foo>();
    return 0;
}
