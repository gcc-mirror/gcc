/* { dg-do compile } */

typedef unsigned size_t;
struct {
    unsigned char buf[sizeof(long)];
} a;
size_t b;
int main()
{
  size_t c, i;
  unsigned char *d;
  for (; c < sizeof(long);)
    {
      d = a.buf;
      b = 0;
      for (; b < i; b++)
	*d++ = '\0';
      for (; c < b; c++)
	*d++ = 'a';
      c = 0;
      for (; i < sizeof(long); i++)
	;
    }
}
