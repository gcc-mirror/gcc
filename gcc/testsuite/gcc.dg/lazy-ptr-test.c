/* { dg-do compile { target powerpc*-apple-darwin* } } */
/* { dg-options "-S" } */

void f () __attribute__((weak_import));

typedef void PF (void);

void f(void){};

PF* g (void) { return f; }

int main()
{
	(*g())();
	return 0;
}

/* { dg-final { scan-assembler "non_lazy_ptr" } } */
