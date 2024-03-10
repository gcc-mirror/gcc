// { dg-do compile }
// { dg-options "-Wshadow" }

void
foo (int x)
{
  int y = 1;
  {
    extern int x;				// { dg-warning "declaration of 'int x' shadows a parameter" }
    extern int y;				// { dg-warning "declaration of 'y' shadows a previous local" }
  }
#if __cplusplus >= 201102L
  auto fn = [x] () { extern int x; return 0; };	// { dg-warning "declaration of 'x' shadows a lambda capture" "" { target c++11 } }
#endif
}

int z;

struct S
{
  int x;
  void foo ()
  {
    extern int x;				// { dg-warning "declaration of 'x' shadows a member of 'S'" }
    extern int z;
  }
};
