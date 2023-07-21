// PR c++/110382
// { dg-do compile { target c++14 } }

struct S {
  double a = 0;
};

constexpr double
g ()
{
  S arr[1];
  S s = arr[0];
  (void) arr[0];
  return s.a;
}

int main() { return  g (); }
