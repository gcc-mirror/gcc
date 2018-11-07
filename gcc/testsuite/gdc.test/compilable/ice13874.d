// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

template FunctionTypeOf(func...)
    if (func.length == 1)
{
    static if (is(typeof(& func[0]) Fsym : Fsym*) && is(Fsym == function) || is(typeof(& func[0]) Fsym == delegate))
    {
        alias Fsym FunctionTypeOf;
    }
    else static if (is(typeof(& func[0].opCall) Fobj == delegate))
    {
        alias Fobj FunctionTypeOf;
    }
    else
        static assert(0);
}

enum DummyEnum;
static assert(!is(FunctionTypeOf!DummyEnum));
