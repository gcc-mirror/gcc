// EXTRA_SOURCES: imports/test21501b.d imports/test21501c.d
// https://issues.dlang.org/show_bug.cgi?id=21501

module test21501a;
import imports.test21501b;
import imports.test21501c;

alias Identity(alias T) = T;

struct A
{
    alias     a = imports.test21501c.C;
    const int b = imports.test21501c.D; // fixed
    alias     c = Identity!(mixin(q{imports.test21501c.C})); // fixed
    const int d = Identity!(mixin(q{imports.test21501c.D})); // fixed

    static assert(is(a == c) && is(a == int));
    static assert(b == d && b == 1);
}
