// PR c++/71442
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-variable" }

struct C
{
  template<typename... Ts>
  int operator()(Ts &&...)
  {
    return sizeof...(Ts);
  }
};

int
foo ()
{
  C {} (1, 1L, 1LL, 1.0);
  return 0;
}

template<int N>
void
bar ()
{
  char a;		// { dg-warning "unused variable" }
  short b;		// { dg-warning "unused variable" }
  int c;		// { dg-warning "unused variable" }
  long d;		// { dg-warning "unused variable" }
  long long e;		// { dg-warning "unused variable" }
  float f;		// { dg-warning "unused variable" }
  double g;		// { dg-warning "unused variable" }
}

void
baz ()
{
  bar <0> ();
}
