// P0145R2: Refining Expression Order for C++
// { dg-do run }
// { dg-options "-std=c++1z" }

extern "C" int printf (const char *, ...);
void sink(...) { }

int last = 0;
int f(int i)
{
  if (i < last)
    __builtin_abort ();
  last = i;
  return i;
}

int& g(int i)
{
  static int dummy;
  f(i);
  return dummy;
}

struct A
{
  int _i;
  A(int i): _i(f(i)) { }
  A& memfn(int i, int j) { f(j); return *this; }
  int operator<<(int i) { }
  A& operator=(const A&) { return *this; }
  A& operator+=(int i) { return *this; }
};

struct B
{
  int _i;
  B(): _i(0) {}
  B(int i): _i(f(i)) { }
};

int operator>>(A&, int i) { }

A a(0);
A* afn(int i)
{
  f(i);
  return &a;
}

A& aref(int i)
{
  f(i);
  return a;
}

B b;
B bval(int i)
{
  return B(i);
}

B& bref(int i)
{
  f(i);
  return b;
}

static int si;
int* ip (int i)
{
  f(i);
  return &si;
}

int& iref(int i)
{
  f(i);
  return si;
}

auto pmff(int i) {
  f(i);
  return &A::memfn;
}

template <class T> void f()
{
  // a.b
  A(1).memfn(f(2),3).memfn(f(4),5);
  aref(6).memfn(f(7),8);
  (aref(9).*pmff(10))(f(11),12);
  last = 0;

  // a->b
  afn(12)->memfn(f(13),14);

  // a->*b
  (afn(15)->*pmff(16))(f(17),18);
  last = 0;

  // a(b)
  // covered in eval-order1.C

  // b @= a
  aref(19)=A(18);
  iref(21)=f(20);
  aref(23)+=f(22);
  bref(25)=bval(24);
  (f(27), b) = bval(26);
  last = 0;

  int ar[4] = {};
  int i = 0;
  ar[i++] = i;
  if (ar[0] != 0)
    __builtin_abort();

  // a[b]
  afn(20)[f(21)-21].memfn(f(22),23);
  ip(24)[f(25)-25] = 0;
  last=0;

  // a << b
  aref(24) << f(25);
  iref(26) << f(27);
  last=0;

  // a >> b
  aref(26) >> f(27);
  iref(28) >> f(29);
}

void g()
{
  // a.b
  A(1).memfn(f(2),3).memfn(f(4),5);
  aref(6).memfn(f(7),8);
  (aref(9).*pmff(10))(f(11),12);
  last = 0;

  // a->b
  afn(12)->memfn(f(13),14);

  // a->*b
  (afn(15)->*pmff(16))(f(17),18);
  last = 0;

  // a(b)
  // covered in eval-order1.C

  // b @= a
  aref(19)=A(18);
  iref(21)=f(20);
  aref(23)+=f(22);
  bref(25)=bval(24);
  (f(27), b) = bval(26);
  last = 0;

  int ar[4] = {};
  int i = 0;
  ar[i++] = i;
  if (ar[0] != 0)
    __builtin_abort();

  // a[b]
  afn(20)[f(21)-21].memfn(f(22),23);
  ip(24)[f(25)-25] = 0;
  last=0;

  // a << b
  aref(24) << f(25);
  iref(26) << f(27);
  last=0;

  // a >> b
  aref(26) >> f(27);
  iref(28) >> f(29);
}

int main()
{
  g();
  last = 0;
  f<int>();
}
