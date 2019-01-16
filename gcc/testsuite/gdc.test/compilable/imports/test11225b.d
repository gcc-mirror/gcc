module imports.test11225b;
import test11225a;

interface J : I {} // remove this line to make it work

static assert(is(typeof({ import imports.test11225c; }))); // OK
pragma(msg, B!().result); // just instantiates the template

template B()
{
    static assert(is(typeof({ import imports.test11225c; }))); // FAILS
    enum result = "WORKS";
}
