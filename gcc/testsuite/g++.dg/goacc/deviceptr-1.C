// { dg-do compile }

template <typename P>
void
func1 (P p)
{
#pragma acc data deviceptr (p)// { dg-bogus "is not a pointer" }
  ;
}

void
func2 (int *p)
{
  func1 (p);
}

template <typename P>
void
func3 (P p)
{
#pragma acc data deviceptr (p)// { dg-error "is not a pointer" }
  ;
}
void
func4 (int p)
{
  func3 (p);
}

template <int N>
void
func5 (int *p, int q)
{
#pragma acc data deviceptr (p)// { dg-bogus "is not a pointer" }
  ;
#pragma acc data deviceptr (q)// { dg-error "is not a pointer" }
  ;
}
