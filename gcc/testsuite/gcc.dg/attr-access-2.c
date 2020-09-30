/* PR 50584 - No warning for passing small array to C99 static array declarator
   Exercise interaction between explicit attribute access and VLA parameters.
   { dg-do compile }
   { dg-options "-Wall" } */

#define RW(...) __attribute__ ((access (read_write, __VA_ARGS__)))


void f1 (int n, int[n], int);               // { dg-message "designating the bound of variable length array argument 2" "note" }

// Verify that a redundant attribute access doesn't trigger a warning.
RW (2, 1) void f1 (int n, int[n], int);

RW (2, 3) void f1 (int n, int[n], int);     // { dg-warning "attribute 'access\\\(read_write, 2, 3\\\)' positional argument 2 conflicts with previous designation by argument 1" }


/* Verify that applying the attribute to a VLA with an unspecified bound
   doesn't trigger any warnings, both with and without a size operand.  */
          void f2 (int, int[*], int);
RW (2)    void f2 (int, int[*], int);
RW (2, 3) void f2 (int, int[*], int);

/* Designating a parameter that comes before the VLA is the same as
   using the standard VLA int[n] syntax.  It might be worth issuing
   a portability warning suggesting to prefer the standard syntax.  */
          void f3 (int, int[*], int);
RW (2, 1) void f3 (int, int[*], int);

/* Designating a parameter that comes after the VLA cannot be expressed
   using the standard VLA int[n] syntax.  Verify it doesn't trigger
   a warning.  */
          void f4 (int, int[*], int);
RW (2, 3) void f4 (int, int[*], int);

/* Also verify the same on the same declaration.  */
          void f5 (int[*], int) RW (1, 2);
RW (1, 2) void f5 (int[*], int);
RW (1, 2) void f5 (int[*], int) RW (1, 2);


/* Verify that designating a VLA parameter with an explicit bound without
   also designating the same bound parameter triggers a warning (it has
   a different meaning).  */
       void f7 (int n, int[n]);         // { dg-message "21:note: designating the bound of variable length array argument 2" "note" }
RW (2) void f7 (int n, int[n]);         // { dg-warning "attribute 'access\\\(read_write, 2\\\)' missing positional argument 2 provided in previous designation by argument 1" }

          void f8 (int n, int[n]);
RW (2, 1) void f8 (int n, int[n]);


          void f9 (int, char[]);        // { dg-message "25:note: previously declared as an ordinary array 'char\\\[]'" note" }
RW (2)    void f9 (int n, char a[n])    // { dg-warning "argument 2 of type 'char\\\[n]' declared as a variable length array" }
                                        // { dg-warning "attribute 'access *\\\(read_write, 2\\\)' positional argument 2 missing in previous designation" "" { target *-*-* } .-1 }
                                        // { dg-message "24:note: designating the bound of variable length array argument 2" "note" { target *-*-* } .-2 }
{ (void)&n; (void)&a; }


          void f10 (int, char[]);       // { dg-message "26:note: previously declared as an ordinary array 'char\\\[]'" "note" }
RW (2, 1) void f10 (int n, char a[n])   // { dg-warning "attribute 'access *\\\(read_write, 2, 1\\\)' positional argument 2 missing in previous designation" "pr????" { xfail *-*-* } }
                                        // { dg-warning "argument 2 of type 'char\\\[n]' declared as a variable length array"  "" { target *-*-* } .-1 }
{ (void)&n; (void)&a; }


/* The following is diagnosed to point out declarations with the T[*]
   form in headers where specifying the bound is just as important as
   in the definition (to detect bugs).  */
          void f11 (int, char[*]);      // { dg-warning "argument 2 of type 'char\\\[\\\*\\\]' declared with 1 unspecified variable bound" }
          void f11 (int m, char a[m]);  // { dg-message "subsequently declared as 'char\\\[m]' with 0 unspecified variable bounds" "note" }
RW (2, 1) void f11 (int n, char arr[n]) // { dg-message "subsequently declared as 'char\\\[n]' with 0 unspecified variable bounds" "note" }
{ (void)&n; (void)&arr; }


/* Verify that redeclaring a function with attribute access applying
   to an array parameter of any form is not diagnosed.  */
          void f12__ (int, int[]) RW (2, 1);
RW (2, 1) void f12__ (int, int[]);

          void f12_3 (int, int[3]) RW (2, 1);
RW (2, 1) void f12_3 (int, int[3]);

          void f12_n (int n, int[n]) RW (2, 1);
RW (2, 1) void f12_n (int n, int[n]);

          void f12_x (int, int[*]) RW (2, 1);
RW (2, 1) void f12_x (int, int[*]);

          void f13__ (int, int[]);
RW (2, 1) void f13__ (int, int[]);

          void f13_5 (int, int[5]);
RW (2, 1) void f13_5 (int, int[5]);

          void f13_n (int n, int[n]);
RW (2, 1) void f13_n (int n, int[n]);

          void f13_x (int, int[*]);
RW (2, 1) void f13_x (int, int[*]);

RW (2, 1) void f14__ (int, int[]);
          void f14__ (int, int[]);

RW (2, 1) void f14_7 (int, int[7]);
          void f14_7 (int, int[7]);

RW (2, 1) void f14_n (int n, int[n]);
          void f14_n (int n, int[n]);

RW (2, 1) void f14_x (int, int[*]);
          void f14_x (int, int[*]);

typedef void G1 (int n, int[n], int);

G1 g1;

/* The warning is about the attribute positional argument 2 which refers
   to the last function argument.  Ideally, the caret would be under
   the corresponding function argument, i.e., the last one here) but
   that location isn't available yet.  Verify that the caret doesn't
   point to function argument 1 which is the VLA bound (that's what
   the caret in the note points to).  */
RW (2, 3) void g1 (int n, int[n], int);     // { dg-warning "16: attribute 'access *\\\(read_write, 2, 3\\\)' positional argument 2 conflicts with previous designation by argument 3" }
// { dg-message "24:designating the bound of variable length array argument 2" "note" { target *-*-* } .-1 }
