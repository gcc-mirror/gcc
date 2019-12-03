// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do run { target c++2a } }

// Test T[]().

int i;
int a1[](1, 2, 3);
static_assert(sizeof(a1) == 3 * sizeof (int), "");
int a2[](1.0, 2, 3);
static_assert(sizeof(a2) == 3 * sizeof (int), "");
int a3[3](1, 2, 3);
int a4[3](1, 2); // a4[2] is value-initialized.
int a5[](++i, ++i);
static_assert(sizeof(a5) == 2 * sizeof (int), "");
int a6[](1);
static_assert(sizeof(a6) == sizeof (int), "");
int a7[]({});
static_assert(sizeof(a7) == sizeof (int), "");
int a8[]({}, {}, {}, {}, {}, {});
static_assert(sizeof(a8) == 6 * sizeof (int), "");
int a9[]((1));
static_assert(sizeof(a9) == sizeof (int), "");
int a10[]((1), (2), (3));
static_assert(sizeof(a10) == 3 * sizeof (int), "");
int a11[][2]{1};
static_assert(sizeof(a11) == 2 * sizeof (int), "");
int a12[][2]({1, 2}, {3, 4});
static_assert(sizeof(a12) == 4 * sizeof (int), "");

const int (&ra1)[](1, 2);
const int (&ra2)[](1.0, 2);
const int (&ra3)[2](1.0, 2);
int (&&rra1)[](1, 2);
int (&&rra2)[](1.0, 2);
int (&&rra3)[2](1.0, 2);

struct S { int i; } s;
S s1[]({1});
static_assert(sizeof(s1) == sizeof (S), "");
S s2[]({1}, {2});
static_assert(sizeof(s2) == 2 * sizeof (S), "");
S s3[]({1}, {2}, {3});
static_assert(sizeof(s3) == 3 * sizeof (S), "");
S s4[3]({1}, {2});
static_assert(sizeof(s4) == 3 * sizeof (S), "");
S s5[]({++i}, {++i});
static_assert(sizeof(s2) == 2 * sizeof (S), "");
S s6[](s, s);

struct R { int i; int j; };
R r1[]({1, 2});
static_assert(sizeof(r1) == sizeof (R), "");
R r2[]({1, 2}, {3, 4});
static_assert(sizeof(r2) == 2 * sizeof (R), "");
R r3[]({1.0, 2}, {3.2, 4});
static_assert(sizeof(r3) == 2 * sizeof (R), "");

char c1[]('r');
char c2[]('r', 'a', 'c', 'c', 'o', 'o', 'n');
char c3[]("yarrow");
char c4[4]("oak");
char c5[10]("cat");
const char (&c6)[4]("eel");

int g;
struct X {
  int i;
  X() { ++g; }
  X(int) { };
};

int
main ()
{
  // Here we'll value-initialize l[1] and l[2], which will zero-initialize it.
  int l[3](42);
  if (l[0] != 42 || l[1] != 0 || l[2] != 0)
    __builtin_abort ();

  // Here we'll value-initialize x[2] and x[3].  Since X is a class type
  // with a user-provided ctor, we'll default-initialize in both cases.
  X x[4]({ 1 }, { 2 });
  if (g != 2)
    __builtin_abort ();

  if (a1[0] != 1 || a1[1] != 2 || a1[2] != 3)
    __builtin_abort ();
  if (a2[0] != 1 || a2[1] != 2 || a2[2] != 3)
    __builtin_abort ();
  if (a3[0] != 1 || a3[1] != 2 || a3[2] != 3)
    __builtin_abort ();
  if (a4[0] != 1 || a4[1] != 2 || a4[2] != 0)
    __builtin_abort ();
  if (a5[0] != 1 || a5[1] != 2)
    __builtin_abort ();
  if (a6[0] != 1)
    __builtin_abort ();
  if (a7[0] != 0)
    __builtin_abort ();
  if (a8[0] != 0 || a8[1] != 0 || a8[2] != 0 || a8[3] != 0
      || a8[4] != 0 || a8[5] != 0)
    __builtin_abort ();
  if (a9[0] != 1)
    __builtin_abort ();
  if (a10[0] != 1 || a10[1] != 2 || a10[2] != 3)
    __builtin_abort ();
  if (a11[0][0] != 1 || a11[0][1] != 0)
    __builtin_abort ();
  if (a12[0][0] != 1 || a12[0][1] != 2 || a12[1][0] != 3 || a12[1][1] != 4)
    __builtin_abort ();

  if (ra1[0] != 1 || ra1[1] != 2)
    __builtin_abort ();
  if (ra2[0] != 1 || ra2[1] != 2)
    __builtin_abort ();
  if (ra3[0] != 1 || ra3[1] != 2)
    __builtin_abort ();
  if (rra1[0] != 1 || rra1[1] != 2)
    __builtin_abort ();
  if (rra2[0] != 1 || rra2[1] != 2)
    __builtin_abort ();
  if (rra3[0] != 1 || rra3[1] != 2)
    __builtin_abort ();

  if (s1[0].i != 1)
    __builtin_abort ();
  if (s2[0].i != 1 || s2[1].i != 2)
    __builtin_abort ();
  if (s3[0].i != 1 || s3[1].i != 2 || s3[2].i != 3)
    __builtin_abort ();
  if (s4[0].i != 1 || s4[1].i != 2 || s4[2].i != 0)
    __builtin_abort ();
  if (s5[0].i != 3 || s5[1].i != 4)
    __builtin_abort ();

  if (r1[0].i != 1 || r1[0].j != 2)
    __builtin_abort ();
  if (r2[0].i != 1 || r2[0].j != 2 || r2[1].i != 3 || r2[1].j != 4)
    __builtin_abort ();
  if (r3[0].i != 1 || r3[0].j != 2 || r3[1].i != 3 || r3[1].j != 4)
    __builtin_abort ();
}
