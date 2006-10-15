/* { dg-do compile } */
/* { dg-options "-O" } */

static int bof __attribute__((used));
int foo()
{
	static int barbarbarbar __attribute__((used));
};

/* { dg-final { scan-assembler "barbarbarbar" } } */
