// EXTRA_FILES: imports/test15371.d
import imports.test15371;

void main()
{
    A a;
    static assert(__traits(hasMember, A, "a"));
    static assert(__traits(getOverloads, A, "fun").length == 3);
    static assert(__traits(compiles, __traits(getMember, a, "a") ));
}
