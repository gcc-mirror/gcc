/* PR middle-end/97189 - ICE on redeclaration of a function with VLA argument
   and attribute access
   Also verify the right arguments are underlined in the notes.
   { dg-do compile }
   { dg-options "-Wall -fdiagnostics-show-caret" } */

#define RW(...) __attribute__ ((access (read_write, __VA_ARGS__)))

RW (2, 3) void f1 (int n, int[n], int);
/* { dg-warning "attribute 'access \\(read_write, 2, 3\\)' positional argument 2 conflicts with previous designation by argument 3" "warning" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 RW (2, 3) void f1 (int n, int[n], int);
                ^~
   { dg-end-multiline-output "" }
   { dg-message "designating the bound of variable length array argument 2" "note" { target *-*-* } .-6 }
   { dg-begin-multiline-output "" }
 RW (2, 3) void f1 (int n, int[n], int);
                    ~~~~^  ~~~~~~
   { dg-end-multiline-output "" } */


RW (2)    void f2 (int, int[*], int);
/* { dg-message "previously declared as a variable length array 'int\\\[\\\*]'" "note" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 RW (2, 3) void f2 (int, int[], int);
                         ^~~~~
   { dg-end-multiline-output "" } */

RW (2, 3) void f2 (int, int[], int);
/* { dg-warning "argument 2 of type 'int\\\[]' declared as an ordinary array" "warning" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 RW (2)    void f2 (int, int[*], int);
                         ^~~~~~
   { dg-end-multiline-output "" } */
