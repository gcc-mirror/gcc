/* { dg-do compile } */

typedef int size_t;
char a;
int main()
{
  size_t b, c;
  for (;;)
    {
      b = 0;
      for (; c;)
	;
      for (; b < sizeof(long); b++)
	;
      for (; b < c; b++)
	a++;
      for (; c < b; c++)
	;
    }
}
