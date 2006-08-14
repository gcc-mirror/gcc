// { dg-do compile }

void f(void)
{
	int a, b;
	(a >? b) = 1; // { dg-error "" }
}


void g(void)
{
	int a, b;
	(a <? b) = 1; // { dg-error "" }
}
