/* { dg-do run } */
/* { dg-additional-options "-fno-forward-propagate" } */
/* { dg-require-effective-target int32plus } */

int a, b;
signed char c, e;
unsigned char d;
__attribute__ ((noinline,noclone,noipa))
void f(int g, short h) {
  for (; a < 2; a++) {
    b = h >> 16;
    e = b * h;
    d = h;
    h = c = d % g;
  }
}
int main()
{ 
  f(129, 128); 
  if (b != -1)
    __builtin_abort ();
}
