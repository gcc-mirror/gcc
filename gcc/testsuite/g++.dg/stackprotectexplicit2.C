/* Test that stack protection is done on chosen functions. */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fstack-protector-explicit" } */

int A()
{
	int A[23];
	char b[22];
	return 0;
}

int __attribute__((stack_protect)) B()
{
	int a;
	int b;
	return a+b;
}

int __attribute__((stack_protect)) c()
{
	int a;
	char b[34];
	return 0;
}


/* { dg-final { scan-assembler-times "stack_chk_fail" 2 } } */
