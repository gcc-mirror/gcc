/* { dg-do compile } */

void bar(unsigned int i)
{
	int a[4];
	char *p = (char*)&a[1] + 4*i;
}
