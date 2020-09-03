// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96152
// { dg-additional-options "-fmain -funittest" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
auto assocArray(Keys, Values)(Keys keys, Values values)
{
    void* aa;
    {
        if (values.length > keys.length)
            values = values[0 .. keys.length];
        else if (keys.length > values.length)
            keys = keys[0 .. values.length];
        aa = aaLiteral(keys, values);
    }
    alias Key = typeof(keys[0]);
    alias Value = typeof(values[0]);
    return (() @trusted => cast(Value[Key]) aa)();
}

@safe unittest
{
    struct ThrowingElement
    {
        int i;
        static bool b;
        ~this(){
            if (b)
                throw new Exception("");
        }
    }
    assert(assocArray([ThrowingElement()], [0]) == [ThrowingElement(): 0]);
}
