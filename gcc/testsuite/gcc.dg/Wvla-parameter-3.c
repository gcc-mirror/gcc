/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   Verify that redeclarations of functions with pointer parameters to
   arrays with variable bounds are diagnosed if the bounds don't match
   either in kind or in the variable expression.
   { dg-do compile }
   { dg-options "-Wall -Wvla-parameter" } */

extern int m, n;

void pa_ (int (*)[]);                   // { dg-message "previously declared as 'int \\\(\\\*\\\)\\\[]'" "note" }
void pa_ (int (*)[n]);                  // { dg-warning "\\\[-Wvla-parameter" }
void pa_ (int (*)[n + 1]);              // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int *\\\(\\\*\\\)\\\[n \\\+ 1\\\]'" }

void ppa_ (int (**)[]);                 // { dg-message "previously declared as 'int \\\(\\\*\\\*\\\)\\\[]'" "note" }
void ppa_ (int (**)[n]);                // { dg-warning "\\\[-Wvla-parameter" }
void ppa_ (int (**)[n + 1]);            // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int \\\(\\\*\\\*\\\)\\\[n \\\+ 1\\\]'" }

void pa1 (int (*)[1]);                  // { dg-message "previously declared as 'int \\\(\\\*\\\)\\\[1]'" "note" }
void pa1 (int (*)[n]);                  // { dg-warning "\\\[-Wvla-parameter" }
void pa1 (int (*)[1]);
void pa1 (int (*)[n + 1]);              // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int *\\\(\\\*\\\)\\\[n \\\+ 1\\\]'" }

void ppax (int (**)[*]);                // { dg-message "previously declared as 'int \\\(\\\*\\\*\\\)\\\[.]'" "note" }
void ppax (int (**)[n]);                // { dg-warning "\\\[-Wvla-parameter" }
/* A VLA with an unspecified bound is represented the same as [0] so
   so the pretty printer can't differentiate between the two forms.  */
void ppax (int (**)[1]);                // { dg-bogus "\\\[-Warray-parameter" "pr100420 (expected)" { xfail *-*-* } }
                                        // { dg-warning "\\\[-Wvla-parameter" "pr100420 (expected)" { xfail *-*-* } .-1 }
void ppax (int (**)[n + 1]);            // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int *\\\(\\\*\\\*\\\)\\\[n \\\+ 1\\\]'" }


void pa1_n (int (*)[1][n]);
void pa1_n (int (*)[1][n]);
void pa1_n (int (*)[*][n]);             // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int \\\(\\\*\\\)\\\[\\\*]\\\[n]'" "pr100420 (expected)" { xfail *-*-*} }
                                        // { dg-warning "mismatch in bound 1 of argument 1 declared as 'int \\\(\\\*\\\)\\\[0]\\\[n]'" "pr100420" { target *-*-* } .-1 }

void pa1_n_2 (int (*)[1][n][2]);
void pa1_n_2 (int (*)[1][n][*]);        // { dg-warning "mismatch in bound 3 of argument 1 declared as 'int \\\(\\\*\\\)\\\[1]\\\[n]\\\[\\\*]'" "pr100420 (expected)" { xfail *-*-* } }
                                        // { dg-warning "mismatch in bound 3 of argument 1 declared as 'int \\\(\\\*\\\)\\\[1]\\\[n]\\\[0]'" "pr100420" { target *-*-* } .-1 }


void pa1_n_2_a1_n_2 (int (*)[1][n][2], int (*)[1][n][2]);
// { dg-message "previously declared as 'int \\\(\\\*\\\)\\\[1]\\\[n]\\\[2]'" "note" { target *-*-* } .-1 }
void pa1_n_2_a1_n_2 (int (*)[1][n][2], int (*)[1][n][n]);
// { dg-warning "mismatch in bound 3 of argument 2 declared as 'int \\\(\\\*\\\)\\\[1]\\\[n]\\\[n]'" "" { target *-*-* } .-1 }
void pa1_n_2_a1_n_2 (int (*)[1][n][2], int (*)[1][3][2]);
// { dg-warning "mismatch in bound 2 of argument 2 declared as 'int \\\(\\\*\\\)\\\[1]\\\[3]\\\[2]'" "" { target *-*-* } .-1 }
void pa1_n_2_a1_n_2 (int (*)[1][n][2], int (*)[n][n][2]);
// { dg-warning "mismatch in bound 1 of argument 2 declared as 'int \\\(\\\*\\\)\\\[n]\\\[n]\\\[2]'" "" { target *-*-* } .-1 }
void pa1_n_2_a1_n_2 (int (*)[1][n][n], int (*)[1][n][2]);
// { dg-warning "mismatch in bound 3 of argument 1 declared as 'int \\\(\\\*\\\)\\\[1]\\\[n]\\\[n]'" "" { target *-*-* } .-1 }
void pa1_n_2_a1_n_2 (int (*)[n][n][2], int (*)[1][n][2]);
// { dg-warning "mismatch in bound 1 of argument 1 declared as 'int \\\(\\\*\\\)\\\[n]\\\[n]\\\[2]'" "" { target *-*-* } .-1 }
void pa1_n_2_a1_n_2 (int (*)[*][*][*], int (*)[*][*][2]);
// { dg-warning "mismatch in bounds 1, 2, 3 of argument 1 declared as 'int \\\(\\\*\\\)\\\[.]\\\[.]\\\[.]'" "" { target *-*-* } .-1 }
// { dg-warning "mismatch in bounds 1, 2 of argument 2 declared as 'int \\\(\\\*\\\)\\\[.]\\\[.]\\\[2]'" "" { target *-*-* } .-2 }
void pa1_n_2_a1_n_2 (int (*)[1][n][2], int (*)[1][n][2]);

/* Verify that pointers to arrays of pointers to arrays...etc are handled
   correctly.  */
void pa2pampan (int (*(*(*(*)[2])[m])[n]));
// { dg-message "previously declared as 'int \\\* \\\(\\\* \\\(\\\* \\\(\\\*\\\)\\\[2]\\\)\\\[m]\\\)\\\[n]'" "note" { target *-*-* } .-1 }
void pa2pampan (int (*(*(*(*)[2])[m])[1]));
// { dg-warning "mismatch in bound 3 of argument 1 declared as 'int \\\* \\\(\\\* \\\(\\\* \\\(\\\*\\\)\\\[2]\\\)\\\[m]\\\)\\\[1]'" "" { target *-*-* } .-1  }
void pa2pampan (int (*(*(*(*)[2])[1])[n]));
// { dg-warning "mismatch in bound 2 of argument 1 declared as 'int \\\* \\\(\\\* \\\(\\\* \\\(\\\*\\\)\\\[2]\\\)\\\[1]\\\)\\\[n]'" "" { target *-*-* } .-1  }
void pa2pampan (int (*(*(*(*)[2])[m])[n]));
