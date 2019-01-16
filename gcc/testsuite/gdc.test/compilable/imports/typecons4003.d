module imports.typecons4003;

struct Tuple(T...)
{
    alias T Types;
    Types field;

    ref Tuple!(Types[from .. to]) slice(uint from, uint to)()
    {
        return *cast(typeof(return) *) &(field[from]);
    }

    void test() //unittest
    {
        .Tuple!(int, string, float, double) a;
        a.field[1] = "abc";
        a.field[2] = 4.5;
        auto s = a.slice!(1, 3);
        static assert(is(typeof(s) == Tuple!(string, float)));
        //assert(s.field[0] == "abc" && s.field[1] == 4.5);
    }
}
