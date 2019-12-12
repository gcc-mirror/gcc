// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89017
// { dg-do compile }

enum Type
{
    Struct,
    Class,
    Pointer,
    Array,
}

auto f89017(Type type)()
{
    static if (type == Type.Class)
    {
        class C(S)
        {
            struct S
            {
                void fn(){}
            }
        }
    }
    else
    {
        struct C(S)
        {
            struct S
            {
                void fn(){}
            }
        }
    }

    static if (type == Type.Struct)
        return C!Type();
    static if (type == Type.Class || type == Type.Pointer)
        return new C!Type();
    static if (type == Type.Array)
        return new C!Type[2];
}

void test89017()
{
    f89017!(Type.Class);
    f89017!(Type.Struct);
    f89017!(Type.Pointer);
    f89017!(Type.Array);
}
