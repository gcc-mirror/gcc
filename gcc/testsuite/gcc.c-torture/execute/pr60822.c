struct X {
    char fill0[800000];
    int a;
    char fill1[900000];
    int b;
};

int __attribute__((noinline,noclone))
Avg(struct X *p, int s)
{
    return (s * (long long)(p->a + p->b)) >> 17;
}

struct X x;

int main()
{
    x.a = 1 << 17;
    x.b = 2 << 17;
    if (Avg(&x, 1) != 3)
	__builtin_abort();
    return 0;
}

