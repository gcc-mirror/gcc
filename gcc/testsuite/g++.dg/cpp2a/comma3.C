// PR c++/91338 - P1161R3: Deprecate a[b,c].
// { dg-do compile { target c++11 } }
// { dg-options "-Wcomma-subscript" }

struct S {
  int operator,(int) { return 42; }
};

void
fn (int *a, int b, int c)
{
  a[b,c]; // { dg-warning "top-level comma expression in array subscript is deprecated" }
  a[(b,c)];

  a[(void) b, c]; // { dg-warning "top-level comma expression in array subscript is deprecated" }
  a[((void) b, c)];

  a[(void) b, (void) c, (void) b, b]; // { dg-warning "top-level comma expression in array subscript is deprecated" }
  a[((void) b, (void) c, (void) b, b)];

  a[S(), 10]; // { dg-warning "top-level comma expression in array subscript is deprecated" }
  a[(S(), 10)];

  a[int{(1,2)}];
  a[int{(1,2)}, int{}]; // { dg-warning "top-level comma expression in array subscript is deprecated" }
  a[(int{(1,2)}, int{})];
}
