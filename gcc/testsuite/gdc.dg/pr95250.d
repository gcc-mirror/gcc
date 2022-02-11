// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=95250
// { dg-do compile }

template Unsigned(T)
{
    static assert(false, "Type " ~ T.stringof ~
                  " does not have an Unsigned counterpart");
}


void* f(T)(T a, T b)
{
        alias UnsignedVoid = Unsigned!(T);
        return cast(T)(cast(T)(cast(UnsignedVoid)(a-b) / 2));
}

static assert(is(typeof(f!(void*)(null, null)) == void*));
// { dg-error "static assert:  \(.*\) is false" "" { target *-*-* } .-1 }
