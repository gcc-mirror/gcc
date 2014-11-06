// { dg-do run { target c++14 } }

struct S { int a; const char* b; int c; int d = b[a]; void *p = this+1; };
constexpr S ss = S(S{ 1, "asdf" });

#define SA(X) static_assert ((X),#X)

SA(ss.a==1);
SA(ss.b[0] == 'a' && ss.b[1] == 's' && ss.b[2] == 'd' && ss.b[3] == 'f');
SA(ss.d == 's');
SA(ss.p == &ss+1);

struct A
{
  struct B {
    int i;
    int j = i+1;
  } b;
  int a = b.j+1;
};

extern constexpr A a = { };
SA(a.b.i == 0 && a.b.j == 1 && a.a == 2);

int f(const A& ar) { return ar.a; }

int main()
{
  S ss2 = { 1, "asdf" };
  if (ss2.a != 1
      || __builtin_strcmp(ss2.b,"asdf") != 0
      || ss2.c != int()
      || ss2.d != 's'
      || ss2.p != &ss2+1)
    __builtin_abort();

  A a = {};
  int i = f(A{});
  if (a.a != 2 || i != 2)
    __builtin_abort();
}
