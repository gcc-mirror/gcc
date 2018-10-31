module imports.link9571a;
struct MapResult(alias fun)
{
    void bar() { fun(0); }
}
auto foo()
{
    alias MapResult!(function(int c) => 0) M;
    M m;
    m.bar();
}
