/* { dg-do compile } */
/* { dg-additional-options "-Wno-stringop-overflow" } */
/* The loop below writes past the end of the global object a.
   When the loop is transformed into a call to memcpy the buffer
   overflow is detected and diagnosed by the -Wstringop-overflow
   option enabled by default.  */

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
