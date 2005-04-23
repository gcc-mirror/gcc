/* { dg-do link } */

void link_error();

int a[4];
long b, c;

int main()
{
	if (&a[b] - &a[c] != b - c)
		link_error();
	return 0;
}
