typedef int (*IsAcceptableThis) (const int );
inline int
fn1 (IsAcceptableThis p1)
{
    p1 (0);
    return 0;
}

__attribute__ ((always_inline))
inline int fn2 (const int a)
{
    return 0;
}

void
fn3 ()
{
    fn1 (fn2);
}
