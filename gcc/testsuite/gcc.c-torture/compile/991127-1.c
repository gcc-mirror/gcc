
extern void foo (int *);

static void bar (char *buf)
{
    int a;
    foo (&a);
    while (a > 0) {
	*buf++ = '0';
	a--;
    }
}
