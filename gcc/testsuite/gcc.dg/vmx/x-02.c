#include <altivec.h>

static vector bool char
g(vector unsigned char, vector bool char);

static int q(void);

static vector bool char
f(vector bool char b, vector unsigned char d) 
{
  vector bool char *p = &b;
  *p = g(d,b);
  return q() ? *p : b;
}

static vector bool char b8;
static vector unsigned char u8;

static vector bool char
g(vector unsigned char a, vector bool char b)
{
  return b8;
}

static int q ()
{
  return 1;
}

int main() 
{ 
  f(b8, u8);
  return 0; 
}
