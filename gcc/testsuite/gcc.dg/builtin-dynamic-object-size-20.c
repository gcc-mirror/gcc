/* PR 109334 
 * { dg-do run }
 * { dg-options "-O1" } */


[[gnu::noinline,gnu::noipa]]
int f(int n, int buf[n])
    [[gnu::access(read_only, 2, 1)]]
{
    return __builtin_dynamic_object_size(buf, 0);
}

[[gnu::noinline,gnu::noipa]]
int g(int n, int buf[])
    [[gnu::access(read_only, 2, 1)]]
{
    return __builtin_dynamic_object_size(buf, 0);
}

[[gnu::noinline,gnu::noipa]]
int h(int n, int buf[n])
{
    return __builtin_dynamic_object_size(buf, 0);
}

int dummy(int x) { return x + 1; }

[[gnu::noinline,gnu::noipa]]
int i(int n, int buf[dummy(n)])
{
    return __builtin_dynamic_object_size(buf, 0);
}

int main()
{
    int n = 10;
    int buf[n];
    if (n * sizeof(int) != f(n, buf))
        __builtin_abort();
    if (n * sizeof(int) != g(n, buf))
        __builtin_abort();
    if (n * sizeof(int) != h(n, buf))
        __builtin_abort();

    (void)i(n, buf);
 
    return 0;
}

