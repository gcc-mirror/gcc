// { dg-do compile }
// { dg-options "-Wunused-label" }
// { dg-require-effective-target indirect_jumps }

extern void f9();

template<int i>
void
f1()
{
  if (i)
    return;

 l1: f9();				// { dg-warning "not used" }
 l3: ; f9();				// { dg-warning "not used" }
 l4: __attribute__ ((unused)) ; f9();
}

template
void f1<0>();

template<int i>
void
f2()
{
  if (i)
    return;

 l1: f9();				// { dg-warning "not used" }
 l3: ; f9();				// { dg-warning "not used" }
 l4: __attribute__ ((unused)) ; f9();
}

template
void f2<1>();

template<int i>
void
f3()
{
  void* lab;
 l1: f9();
 l2: __attribute__ ((unused)) ; f9();
  lab = i ? &&l1 : &&l2;
  goto *lab;
}

template
void f3<0>();

template
void f3<1>();
