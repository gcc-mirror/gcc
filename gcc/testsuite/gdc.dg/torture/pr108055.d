// { dg-do link }
// { dg-additional-files "imports/pr108055conv.d imports/pr108055spec.d imports/pr108055write.d" }
// { dg-additional-options "-I[srcdir] -fno-druntime" }
import imports.pr108055conv;

extern(C) int main()
{
    float zis;
    static if (is(typeof(to!string(&zis))))
        to!string(&zis);
    return 0;
}
