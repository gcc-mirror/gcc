struct Test
{
    struct opDispatch(string dummy)
    { enum opDispatch = 1; }
}
auto temp = Test().foo!(int);
