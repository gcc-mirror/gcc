// PERMUTE_ARGS:
// https://github.com/dlang/dmd/pull/5860

int[] bar() @safe;

void foo(int[] a) @safe
{
    static int[] as;

    bool b;

    b = a.ptr == null;
    b = null == (*&as).ptr;
    b = bar().ptr == null;

    b = a.ptr != null;
    b = null != (*&as).ptr;
    b = bar().ptr != null;

    b = a.ptr is null;
    b = null is (*&as).ptr;
    b = bar().ptr is null;

    b = a.ptr !is null;
    b = null !is (*&as).ptr;
    b = bar().ptr !is null;

    b = !a.ptr;
    b = !(*&as).ptr;
    b = !bar().ptr;

    b = cast(bool)a.ptr;
    b = cast(bool)(*&as).ptr;
    b = cast(bool)bar().ptr;

    b = a.ptr ? false : true;

    b = a.ptr < null;
    b = null < a.ptr;

    b = a.ptr && null || a.ptr;

    if (a.ptr)
	b = true;

    while (a.ptr)
	b = true;

    for (; a.ptr;)
	b = true;

//    ptrdiff_t d = a.ptr - a.ptr;
}
