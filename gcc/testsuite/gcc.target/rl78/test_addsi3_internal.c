/* { dg-do compile } */
/* { dg-options "-Os" } */
long l, v;

void test1()
{
	l++;
}

void test2()
{
	l--;
}

void test3()
{
	l += 10;
}

long test4()
{
	return l + v;
}

/* { dg-final { scan-assembler-not "addw ax, #0" } } */
/* { dg-final { scan-assembler-not "addw ax, #-1" } } */
/* { dg-final { scan-assembler "decw ax" } } */
