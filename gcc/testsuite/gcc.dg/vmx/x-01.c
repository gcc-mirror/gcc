#include <altivec.h>
vector bool char
g(vector unsigned char, vector bool char);

vector bool char
f(vector bool char b, vector unsigned char d) 
{
  vector bool char *p = &b;
  *p = g(d,b);
  return *p;
}

vector bool char b8;
vector unsigned char u8;
vector bool char
g(vector unsigned char a, vector bool char b)
{
  return b8;
}

int main() 
{ 
  f(b8, u8);
  return 0; 
}
