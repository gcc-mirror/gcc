/* { dg-do compile }
/* { dg-options "-O2 -fsee" } */

int f(const char* ptr, int bar) {
  return (((const char *)0 - ptr ) & (bar - 1)) == 0;
}


int g(const char* ptr, const char *test, int N, int bar)  {
  if (N == 0) {
  }
  else if (N > 0) {
    int count = 0;
    while ( count < N) {
      if (!f(ptr, bar))
        count++;
    }
  }
  return f(test, bar) ;
}
