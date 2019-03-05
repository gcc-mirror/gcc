// PERMUTE_ARGS:

// 9209

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
