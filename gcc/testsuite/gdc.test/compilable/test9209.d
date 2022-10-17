// PERMUTE_ARGS:

// https://issues.dlang.org/show_bug.cgi?id=9209

auto array(T)(T t){ return t; }

auto bar()(in int* x)
{
    if (true) return 0;
    return array(bar(x));
}

void main ()
{
    bar(null);
}
