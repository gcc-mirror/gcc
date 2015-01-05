// Contributed by Iain Sandoe <iain@codesourcery.com>, December 2014.  */
// { dg-do compile }
// { dg-options "-std=c++11" }

extern "C" {
  int printf (const char *,...);
}

int main () 
{
  auto f = [] (const char *msg) -> int { printf("%s", msg); return 0; };
  return f("Some test\n");
}
