/* PR middle-end/97189 - ICE on redeclaration of a function with VLA argument
   and attribute access
   { dg-do compile }
   { dg-options "-Wall" } */

#define RW(...) __attribute__ ((access (read_write, __VA_ARGS__)))

RW (2, 3) void f1 (int n, int[n], int);
/* { dg-warning "attribute 'access \\(read_write, 2, 3\\)' positional argument 2 conflicts with previous designation by argument 3" "warning" { target *-*-* } .-1 }
   { dg-message "designating the bound of variable length array argument 2" "note" { target *-*-* } .-2 } */

void call_f1 (int *p)
{
  /* Verify that a warning is issued.  Ideally, it seems the VLA bound
     should take precedence over the attribute and the warning would
     reference argument 1 but since the conflict in the redeclarations
     of the function is already diagnosed don't test that (and let it
     be acceptable for this warning to reference argument 3).  */
  f1 (-1, p, -1);
  // { dg-warning "argument \\d value -1 is negative" "warning" { target *-*-* } .-1 }
}

RW (2)    void f2 (int, int[*], int);
// { dg-message "previously declared as a variable length array 'int\\\[\\\*]'" "note" { target *-*-* } .-1 }
RW (2, 3) void f2 (int, int[], int);
// { dg-warning "argument 2 of type 'int\\\[]' declared as an ordinary array" "warning" { target *-*-* } .-1 }

void call_f2 (int *p)
{
  f2 (-1, p, 0);

  /* Verify that the attribute access on the redeclaration of f2() takes
     precedence over the one on the first declaration.  */
  f2 (0, p, -1);
  // { dg-warning "argument 3 value -1 is negative" "warning" { target *-*-* } .-1 }
}
