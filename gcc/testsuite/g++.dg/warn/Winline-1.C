// { dg-options "-Winline -O2 -fno-unit-at-a-time" }

static inline int foo(int x); // { dg-warning "" }

int main()
{
  return(foo(17)); // { dg-warning "" }
}

inline int foo(int x) {  return(x);  }
