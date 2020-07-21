// REQUIRED_ARGS: -main
class A()
{
    static struct S { A a; }
}
enum e = is(A!());
