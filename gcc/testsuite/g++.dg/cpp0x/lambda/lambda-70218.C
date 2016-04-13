// PR c++/70218
// { dg-do compile { target c++11 } }

struct X {
private:
   int i;
};

struct Y {
  Y (int) { }
};

void
foo ()
{
  Y ([] { X x; x.i = 3; return 0; } ()); // { dg-error "private" }
}
