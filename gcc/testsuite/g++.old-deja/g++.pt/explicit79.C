// { dg-do assemble  }

template <int I>
void f(int (*)[I] = 0);

template <int J>
void f();

void g()
{
  f<-1>();
}
