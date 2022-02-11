/* REQUIRED_ARGS: -preview=rvaluerefparam
 */

struct AS
{
    string get() @safe @nogc pure nothrow { return _s; }
    alias get this;
    @disable this(this);
    string _s;
}

void popFront(ref string) { }
static assert(!is(typeof((R r) => r.popFront)));
