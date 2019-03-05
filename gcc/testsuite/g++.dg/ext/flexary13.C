// { dg-do compile }
// { dg-options -Wno-pedantic }

#define STR(s) #s
#define ASSERT(exp) \
  ((exp) ? (void)0 : (void)(__builtin_printf ("%s:%i: assertion %s failed\n", \
                     __FILE__, __LINE__, STR(exp)), \
                      __builtin_abort ()))

struct Ax { int n, a[]; };
struct AAx { int i; Ax ax; };

int i = 12345678;

int main ()
{
  {
    Ax s = { 0 };
    ASSERT (s.n == 0);
  }
  {
    static Ax s =
      { 0, { } };   // dg-warning "initialization of a flexible array member" }
    ASSERT (s.n == 0);
  }
  {
    static Ax s =
      { 1, { 2 } };   // dg-warning "initialization of a flexible array member" }
    ASSERT (s.n == 1 && s.a [0] == 2);
  }
  {
    static Ax s =
      { 2, { 3, 4 } }; // dg-warning "initialization of a flexible array member" }
    ASSERT (s.n = 2 && s.a [0] == 3 && s.a [1] == 4);
  }
  {
    static Ax s =
      { 123, i };   // dg-warning "initialization of a flexible array member" }
    ASSERT (s.n == 123 && s.a [0] == i);
  }
  {
    static Ax s =
      { 456, { i } }; // dg-warning "initialization of a flexible array member" }
    ASSERT (s.n == 456 && s.a [0] == i);
  }
  {
    int j = i + 1, k = j + 1;
    static Ax s =
      { 3, { i, j, k } }; // dg-warning "initialization of a flexible array member" }
    ASSERT (s.n == 3 && s.a [0] == i && s.a [1] == j && s.a [2] == k);
  }

  {
    AAx s =
      { 1, { 2 } };   // dg-warning "initialization of a flexible array member" }
    ASSERT (s.i == 1 && s.ax.n == 2);
  }
}
