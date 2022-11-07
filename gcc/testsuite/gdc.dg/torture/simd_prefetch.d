// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.simd;

int testprefetch(byte a)
{
    prefetch!(false, 0)(&a);
    prefetch!(false, 1)(&a);
    prefetch!(false, 2)(&a);
    prefetch!(false, 3)(&a);
    prefetch!(true, 0)(&a);
    prefetch!(true, 1)(&a);
    prefetch!(true, 2)(&a);
    prefetch!(true, 3)(&a);
    return 3;
}

void main()
{
    int i = testprefetch(1);
    assert(i == 3);
}
