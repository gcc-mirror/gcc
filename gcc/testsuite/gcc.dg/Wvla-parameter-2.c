/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   Verify the -Wvla-parameter warnings correctly diagnose mismatches
   between multimensional array arguments with one or more variable
   bounds in redeclarations of the same function.
   { dg-do compile }
   { dg-options "-Wall -Wvla-parameter" } */

void fmn_a1n_axn (int n, int[1][n]);            // { dg-message "previously declared as 'int\\\[1]\\\[n]' with 1 variable bound" "note" }
void fmn_a1n_axn (int n, int[*][n]);            // { dg-warning "argument 2 of type 'int\\\[\\\*]\\\[n]' declared with 2 variable bounds" }


void fmn_axn_a2n (int n, int[*][n]);            // { dg-message "previously declared as 'int\\\[\\\*]\\\[n]' with 2 variable bounds" "note" }
void fmn_axn_a2n (int n, int[2][n]);            // { dg-warning "argument 2 of type 'int\\\[2]\\\[n]' declared with 1 variable bound" }


void fmn_amn_axn (int m, int n, int[m][n]);     // { dg-message "previously declared as 'int\\\[m]\\\[n]' with 0 unspecified variable bounds" "note" }
void fmn_amn_axn (int m, int n, int[*][n]);     // { dg-warning "argument 3 of type 'int\\\[\\\*]\\\[n]' declared with 1 unspecified variable bound" }

// Same as above but a different function name.
void gmn_amn_axn (int m, int n, int[m][n]);     // { dg-message "previously declared as 'int\\\[m]\\\[n]' with 0 unspecified variable bounds" "note" }
void gmn_amn_axn (int m, int n, int[*][n]);     // { dg-warning "argument 3 of type 'int\\\[\\\*]\\\[n]' declared with 1 unspecified variable bound" }

typedef int A7[7];

void fm_A7_m_5 (int m, A7[m][5]);               // { dg-message "previously declared as 'int\\\[m]\\\[5]\\\[7]' with bound argument 1" "note" }
void fm_A7_m_5 (int n, A7[n][5]);

void fm_A7_m_5 (int n, A7[n + 1][5]);           // { dg-warning "argument 2 of type 'int\\\[n \\\+ 1]\\\[5]\\\[7]' declared with mismatched bound 'n \\\+ 1'" }


int n1, n2, n3, n4, n5, n6, n7, n8, n9;
void f (int[n1][2][n3][4][n5][6][n7][8][n9]);   // { dg-message "previously declared as 'int\\\[n1]\\\[2]\\\[n3]\\\[4]\\\[n5]\\\[6]\\\[n7]\\\[8]\\\[n9]' with 0 unspecified variable bounds" "note" }
                                                // { dg-message "with 5 variable bounds" "note" { target *-*-* } .-1  }
void f (int[n1][2][n3][4][n5][6][n7][8][n9]);

void f (int[n1][2][n3][4][n5][6][n7][8][*]);    // { dg-warning "argument 1 of type 'int\\\[n1]\\\[2]\\\[n3]\\\[4]\\\[n5]\\\[6]\\\[n7]\\\[8]\\\[\\\*]' declared with 1 unspecified variable bound" "pr100420 (expected)" { target *-*-* } }
void f (int[n1][2][n3][4][n5][6][*][8][n9]);   // { dg-warning "argument 1 of type 'int\\\[n1]\\\[2]\\\[n3]\\\[4]\\\[n5]\\\[6]\\\[\\\*]\\\[8]\\\[n9]' declared with 1 unspecified variable bound" "pr100420 (expected)" { target *-*-* } }
void f (int[n1][2][n3][4][*][6][n7][8][n9]);   // { dg-warning "argument 1 of type 'int\\\[n1]\\\[2]\\\[n3]\\\[4]\\\[\\\*]\\\[6]\\\[n7]\\\[8]\\\[n9]' declared with 1 unspecified variable bound" "pr100420 (expected)" { target *-*-*} }
void f (int[n1][2][*][4][n5][6][n7][8][n9]);   // { dg-warning "argument 1 of type 'int\\\[n1]\\\[2]\\\[\\\*]\\\[4]\\\[n5]\\\[6]\\\[n7]\\\[8]\\\[n9]' declared with 1 unspecified variable bound" "pr100420 (expected)" { target *-*-* } }
void f (int[*][2][n3][4][n5][6][n7][8][n9]);   // { dg-warning "argument 1 of type 'int\\\[\\\*]\\\[2]\\\[n3]\\\[4]\\\[n5]\\\[6]\\\[n7]\\\[8]\\\[n9]' declared with 1 unspecified variable bound" }

void f (int[n1][n2][n3][n4][n5][n6][n7][n8][n9]);   // { dg-warning "argument 1 of type 'int\\\[n1]\\\[n2]\\\[n3]\\\[n4]\\\[n5]\\\[n6]\\\[n7]\\\[n8]\\\[n9]' declared with 9 variable bounds" }

// Verify that arrays of pointers to arrays...etc are handled correctly.
void a2pampan (int (*(*(*[2])[n1])[n2]));
// { dg-message "previously declared as 'int \\\* \\\(\\\* \\\(\\\*\\\[2]\\\)\\\[n1]\\\)\\\[n2]'" "note" { target *-*-* } .-1 }
void a2pampan (int (*(*(*[2])[n1])[1]));
// { dg-warning "argument 1 of type 'int \\\* \\\(\\\* \\\(\\\*\\\[2]\\\)\\\[n1]\\\)\\\[1]' declared with 1 variable bound" "" { target *-*-* } .-1  }
void a2pampan (int (*(*(*[2])[1])[n2]));
// { dg-warning "argument 1 of type 'int \\\* \\\(\\\* \\\(\\\*\\\[2]\\\)\\\[1]\\\)\\\[n2]' declared with 1 variable bound" "" { target *-*-* } .-1  }
void a2pampan (int (*(*(*[2])[n1])[n1]));
// { dg-warning "argument 1 of type 'int \\\* \\\(\\\* \\\(\\\*\\\[2]\\\)\\\[n1]\\\)\\\[n1]' declared with mismatched bound 'n1'" "" { target *-*-* } .-1  }
void a2pampan (int (*(*(*[2])[n1])[n2]));


/* Verify that the presence or absence of static with VLA dooesn't cause
   unwanted warnings.  */

int f2ia1_1 (int n, int [n][n]);            // { sg-message "previously declared as 'int\\\[n]\\\[n]' with bound argument 1" }
int f2ia1_1 (int n, int[static n][n]);
int f2ia1_1 (int n, int a[static n][n]) { return sizeof *a; }
int f2ia1_1 (int n, int[static n + 1][n]);  // { dg-warning "argument 2 of type 'int\\\[static  *n \\\+ 1]\\\[n]' declared with mismatched bound 'n \\\+ 1'" }

int f2ias1_1 (int n, int [static n][n]);    // { dg-message "previously declared as 'int\\\[static +n]\\\[n]' with bound argument 1" }
int f2ias1_1 (int n, int[n][n]);
int f2ias1_1 (int n, int a[++n][n])         // { dg-warning "argument 2 of type 'int\\\[\\\+\\\+n]\\\[n]' declared with mismatched bound ' ?\\+\\+n'" }
{ return sizeof *a; }
