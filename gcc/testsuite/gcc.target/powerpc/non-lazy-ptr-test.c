/* { dg-do compile { target powerpc*-apple-darwin* } } */
/* { dg-options "-S" } */

typedef void PF (void);

static void f(void) {
}

void f1(void) {
}

extern void f2(void) {
}

static void f3(void);

void pe(void)
{
}

PF* g (void) { f(); return f; }
PF* x (void) { return f1; }
PF* y (void) { f2(); return f2; }
PF* z (void) { return f3; }
PF* w (void) { pe(); return pe; }

int main()
{
	(*g())();
	(*x())();
	(*y())();
	(*z())();
	(*w())();
	return 0;
}

void f3(void) {
}

/* { dg-final { scan-assembler-not "non_lazy_ptr" } } */
