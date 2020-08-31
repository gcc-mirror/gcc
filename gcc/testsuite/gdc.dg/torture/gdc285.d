// https://bugzilla.gdcproject.org/show_bug.cgi?id=285
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

inout(char)[] test285(inout(char)* s) @nogc @system pure nothrow
{
    import core.stdc.string : strlen;
    return s ? s[0 .. strlen(s)] : null;
}

void main()
{
    assert(test285(null) == null);
    assert(test285("foo") == "foo");
}
