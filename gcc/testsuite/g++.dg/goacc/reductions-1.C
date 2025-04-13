// Test for invalid reduction variables.

class C1
{
  int b, d[10];

public:
  int a, c[10];

  C1 () { a = 0; b = 0; }
  int& get_b () { return b; }
  int* get_d () { return d; }
};

template <typename T>
class C2
{
  T b, d[10];

public:
  T a, c[10];

  C2 () { a = 0; b = 0; }
  T& get_b () { return b; }
  T* get_d () { return d; }
};

struct S1
{
  int a, b, c[10], d[10];

  S1 () { a = 0; b = 0; }
  int& get_b () { return b; }
  int* get_d () { return d; }
};

template <typename T>
struct S2
{
  T a, b, c[10], d[10];

  S2 () { a = 0; b = 0; }
  T& get_b () { return b; }
  T* get_d () { return d; }
};

template <typename T>
void
test_parallel ()
{
  int i, a[10];
  T b[10];
  C1 c1, c1a[10];
  C2<T> c2, c2a[10];
  S1 s1, s1a[10];
  S2<float> s2, s2a[10];

  // Reductions on class members.

#pragma acc parallel reduction(+:c1.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.a += 1;

#pragma acc parallel reduction(+:c1.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.get_b () += 1;

#pragma acc parallel reduction(+:c1.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.c[1] += 1;

#pragma acc parallel reduction(+:c1.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.get_d ()[1] += 1;

#pragma acc parallel reduction(+:c1a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].a += 1;

#pragma acc parallel reduction(+:c1a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].get_b () += 1;

#pragma acc parallel reduction(+:c1a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].c[1] += 1;

#pragma acc parallel reduction(+:c1a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].get_d ()[1] += 1;


  // Reductions on a template class member.

#pragma acc parallel reduction(+:c2.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.a += 1;

#pragma acc parallel reduction(+:c2.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.get_b () += 1;

#pragma acc parallel reduction(+:c2.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.c[1] += 1;

#pragma acc parallel reduction(+:c2.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.get_d ()[1] += 1;


#pragma acc parallel reduction(+:c2a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].a += 1;

#pragma acc parallel reduction(+:c2a[1].get_b ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].get_b () += 1;

#pragma acc parallel reduction(+:c2a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].c[1] += 1;

#pragma acc parallel reduction(+:c2a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].get_d ()[1] += 1;


  // Reductions on struct element.

#pragma acc parallel reduction(+:s1.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.a += 1;

#pragma acc parallel reduction(+:s1.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.get_b () += 1;

#pragma acc parallel reduction(+:s1.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.c[1] += 1;

#pragma acc parallel reduction(+:s1.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.get_d ()[1] += 1;

#pragma acc parallel reduction(+:s1a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].a += 1;

#pragma acc parallel reduction(+:s1a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].get_b () += 1;

#pragma acc parallel reduction(+:s1a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].c[1] += 1;

#pragma acc parallel reduction(+:s1a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].get_d ()[1] += 1;


  // Reductions on a template struct element.

#pragma acc parallel reduction(+:s2.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.a += 1;

#pragma acc parallel reduction(+:s2.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.get_b () += 1;

#pragma acc parallel reduction(+:s2.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.c[1] += 1;

#pragma acc parallel reduction(+:s2.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.get_d ()[1] += 1;

#pragma acc parallel reduction(+:s2a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].a += 1;

#pragma acc parallel reduction(+:s2a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].get_b () += 1;

#pragma acc parallel reduction(+:s2a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].c[1] += 1;

#pragma acc parallel reduction(+:s2a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].get_d ()[1] += 1;


  // Reductions on arrays.

#pragma acc parallel reduction(+:a[10]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    a[10] += 1;

#pragma acc parallel reduction(+:b[10]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    b[10] += 1;
}

template <typename T>
void
test_combined ()
{
  int i, a[10];
  T b[10];
  C1 c1, c1a[10];
  C2<T> c2, c2a[10];
  S1 s1, s1a[10];
  S2<float> s2, s2a[10];

  // Reductions on class members.

#pragma acc parallel loop reduction(+:c1.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.a += 1;

#pragma acc parallel loop reduction(+:c1.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.get_b () += 1;

#pragma acc parallel loop reduction(+:c1.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.c[1] += 1;

#pragma acc parallel loop reduction(+:c1.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c1.get_d ()[1] += 1;

#pragma acc parallel loop reduction(+:c1a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].a += 1;

#pragma acc parallel loop reduction(+:c1a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].get_b () += 1;

#pragma acc parallel loop reduction(+:c1a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].c[1] += 1;

#pragma acc parallel loop reduction(+:c1a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c1a[1].get_d ()[1] += 1;


  // Reductions on a template class member.

#pragma acc parallel loop reduction(+:c2.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.a += 1;

#pragma acc parallel loop reduction(+:c2.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.get_b () += 1;

#pragma acc parallel loop reduction(+:c2.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.c[1] += 1;

#pragma acc parallel loop reduction(+:c2.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    c2.get_d ()[1] += 1;


#pragma acc parallel loop reduction(+:c2a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].a += 1;

#pragma acc parallel loop reduction(+:c2a[1].get_b ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].get_b () += 1;

#pragma acc parallel loop reduction(+:c2a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].c[1] += 1;

#pragma acc parallel loop reduction(+:c2a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    c2a[1].get_d ()[1] += 1;


  // Reductions on struct element.

#pragma acc parallel loop reduction(+:s1.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.a += 1;

#pragma acc parallel loop reduction(+:s1.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.get_b () += 1;

#pragma acc parallel loop reduction(+:s1.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.c[1] += 1;

#pragma acc parallel loop reduction(+:s1.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s1.get_d ()[1] += 1;

#pragma acc parallel loop reduction(+:s1a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].a += 1;

#pragma acc parallel loop reduction(+:s1a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].get_b () += 1;

#pragma acc parallel loop reduction(+:s1a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].c[1] += 1;

#pragma acc parallel loop reduction(+:s1a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s1a[1].get_d ()[1] += 1;


  // Reductions on a template struct element.

#pragma acc parallel loop reduction(+:s2.a) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.a += 1;

#pragma acc parallel loop reduction(+:s2.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.get_b () += 1;

#pragma acc parallel loop reduction(+:s2.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.c[1] += 1;

#pragma acc parallel loop reduction(+:s2.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
  for (i = 0; i < 100; i++)
    s2.get_d ()[1] += 1;

#pragma acc parallel loop reduction(+:s2a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].a += 1;

#pragma acc parallel loop reduction(+:s2a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].get_b () += 1;

#pragma acc parallel loop reduction(+:s2a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].c[1] += 1;

#pragma acc parallel loop reduction(+:s2a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    s2a[1].get_d ()[1] += 1;


  // Reductions on arrays.

#pragma acc parallel loop reduction(+:a[10]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    a[10] += 1;

#pragma acc parallel loop reduction(+:b[10]) // { dg-error "expected '\\\)' before '\\\[' token" }
  for (i = 0; i < 100; i++)
    b[10] += 1;
}

template <typename T>
void
test_loop ()
{
  int i, a[10];
  T b[10];
  C1 c1, c1a[10];
  C2<T> c2, c2a[10];
  S1 s1, s1a[10];
  S2<float> s2, s2a[10];

  // Reductions on class members.

  #pragma acc parallel
  {

#pragma acc loop reduction(+:c1.a) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c1.a += 1;

#pragma acc loop reduction(+:c1.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c1.get_b () += 1;

#pragma acc loop reduction(+:c1.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c1.c[1] += 1;

#pragma acc loop reduction(+:c1.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c1.get_d ()[1] += 1;

#pragma acc loop reduction(+:c1a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c1a[1].a += 1;

#pragma acc loop reduction(+:c1a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c1a[1].get_b () += 1;

#pragma acc loop reduction(+:c1a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c1a[1].c[1] += 1;

#pragma acc loop reduction(+:c1a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c1a[1].get_d ()[1] += 1;


    // Reductions on a template class member.

#pragma acc loop reduction(+:c2.a) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c2.a += 1;

#pragma acc loop reduction(+:c2.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c2.get_b () += 1;

#pragma acc loop reduction(+:c2.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c2.c[1] += 1;

#pragma acc loop reduction(+:c2.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      c2.get_d ()[1] += 1;


#pragma acc loop reduction(+:c2a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c2a[1].a += 1;

#pragma acc loop reduction(+:c2a[1].get_b ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c2a[1].get_b () += 1;

#pragma acc loop reduction(+:c2a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c2a[1].c[1] += 1;

#pragma acc loop reduction(+:c2a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      c2a[1].get_d ()[1] += 1;


    // Reductions on struct element.

#pragma acc loop reduction(+:s1.a) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s1.a += 1;

#pragma acc loop reduction(+:s1.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s1.get_b () += 1;

#pragma acc loop reduction(+:s1.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s1.c[1] += 1;

#pragma acc loop reduction(+:s1.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s1.get_d ()[1] += 1;

#pragma acc loop reduction(+:s1a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s1a[1].a += 1;

#pragma acc loop reduction(+:s1a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s1a[1].get_b () += 1;

#pragma acc loop reduction(+:s1a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s1a[1].c[1] += 1;

#pragma acc loop reduction(+:s1a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s1a[1].get_d ()[1] += 1;


    // Reductions on a template struct element.

#pragma acc loop reduction(+:s2.a) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s2.a += 1;

#pragma acc loop reduction(+:s2.get_b ()) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s2.get_b () += 1;

#pragma acc loop reduction(+:s2.c[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s2.c[1] += 1;

#pragma acc loop reduction(+:s2.get_d ()[1]) // { dg-error "expected '\\\)' before '\\\.' token" }
    for (i = 0; i < 100; i++)
      s2.get_d ()[1] += 1;

#pragma acc loop reduction(+:s2a[1].a) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s2a[1].a += 1;

#pragma acc loop reduction(+:s2a[1].get_b ()) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s2a[1].get_b () += 1;

#pragma acc loop reduction(+:s2a[1].c[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s2a[1].c[1] += 1;

#pragma acc loop reduction(+:s2a[1].get_d ()[1]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      s2a[1].get_d ()[1] += 1;


    // Reductions on arrays.

#pragma acc loop reduction(+:a[10]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      a[10] += 1;

#pragma acc loop reduction(+:b[10]) // { dg-error "expected '\\\)' before '\\\[' token" }
    for (i = 0; i < 100; i++)
      b[10] += 1;
  }
}

int
main ()
{
  test_parallel<double> ();
  test_combined<long> ();
  test_loop<short> ();

  return 0;
}
