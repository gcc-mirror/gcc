/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   Verify the -Wvla-parameter warnings correctly diagnose mismatches
   between one-dimensional VLA and non-VLA arguments in redeclarations
   of the same function.
   Also verify that the array/pointer argument form in a mismatched
   redeclaration doesn't override the form in the initial declaration.
   { dg-do compile }
   { dg-options "-Wall -Wvla-parameter" } */

/* Verify that redeclaring an argument as a VLA with an unspecified
   bound that was first declared as an ordinary array with an unspecified
   bound triggers a warning.  */
void f1ia_x (int[]);          // { dg-message "previously declared as an ordinary array 'int\\\[]'" "note" }
void f1ia_x (int[*]);         // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
void f1ia_x (int[]);
void f1ia_x (int[*]);         // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
/* Also verify that a definition of the same form as the first declaration
   doesn't trigger a warning and doesn't prevent warnings for subsequent
   mismatches.  */
void f1ia_x (int a[]) { (void)&a;}
void f1ia_x (int[*]);         // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }

/* Repeat the above but starting with an ordinary array with a constant
   bound.  */
void f1ia1x (int[1]);          // { dg-message "previously declared as an ordinary array 'int\\\[1]'" "note" }
void f1ia1x (int[*]);         // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
void f1ia1x (int a[1]) { (void)&a; }
void f1ia1x (int[1]);
void f1ia1x (int[*]);         // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }

void f1ipx (int*);            // { dg-message "previously declared as a pointer 'int ?\\\*'" "note" }
void f1ipx (int[*]);          // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
void f1ipx (int*);
void f1ipx (int *p) { (void)&p; }
void f1ipx (int[*]);          // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
void f1ipx (int*);

void f2ipx (int*, int*);      // { dg-message "previously declared as a pointer 'int ?\\\*'" "note" }
void f2ipx (int*, int[*]);    // { dg-warning "argument 2 of type 'int\\\[\\\*]' declared as a variable length array" }
void f2ipx (int*, int*);
void f2ipx (int*, int[*]);    // { dg-warning "argument 2 of type 'int\\\[\\\*]' declared as a variable length array" }
void f2ipx (int *p, int *q) { (void)&p; (void)&q; }
void f2ipx (int*, int[*]);    // { dg-warning "argument 2 of type 'int\\\[\\\*]' declared as a variable length array" }

void f1ias2x (int[static 2]); // { dg-message "previously declared as an ordinary array 'int\\\[static 2]'" }
void f1ias2x (int[*]);        // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
void f1ias2x (int[static 2]);
void f1ias2x (int[*]);        // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
void f1ias2x (int a[static 2]) { (void)&a; }
void f1ias2x (int[*]);        // { dg-warning "argument 1 of type 'int\\\[\\\*]' declared as a variable length array" }
void f1ias2x (int[static 2]);

extern int nelts;

void f1sa_var (short[]);      // { dg-message "previously declared as an ordinary array 'short int\\\[]'" }
void f1sa_var (short[nelts]); // { dg-warning "argument 1 of type 'short int\\\[nelts]' declared as a variable length array" }
void f1sa_var (short[]);
void f1sa_var (short[nelts]); // { dg-warning "argument 1 of type 'short int\\\[nelts]' declared as a variable length array" }
void f1sa_var (short a[]) { (void)&a; }
void f1sa_var (short[nelts]); // { dg-warning "argument 1 of type 'short int\\\[nelts]' declared as a variable length array" }
void f1sa_var (short[]);

void f1sa_expr (int[]);           // { dg-message "previously declared as an ordinary array 'int\\\[]'" }
void f1sa_expr (int[nelts + 1]);  // { dg-warning "argument 1 of type 'int\\\[nelts \\\+ 1]' declared as a variable length array" }
void f1sa_expr (int[]);
void f1sa_expr (int[nelts * 2]);  // { dg-warning "argument 1 of type 'int\\\[nelts \\\* 2]' declared as a variable length array" }
void f1sa_expr (int a[]) { (void)&a; }
void f1sa_expr (int[nelts / 3]);  // { dg-warning "argument 1 of type 'int\\\[nelts / 3]' declared as a variable length array" }
void f1sa_expr (int[]);

extern int f (int);

void f1ia_f (int[]);          // { dg-message "previously declared as an ordinary array 'int\\\[]'" }
void f1ia_f (int[f (1)]);     // { dg-warning "argument 1 of type 'int\\\[f *\\\(1\\\)]' declared as a variable length array" }
void f1ia_f (int[]);
void f1ia_f (int[f (2)]);     // { dg-warning "argument 1 of type 'int\\\[f *\\\(2\\\)]' declared as a variable length array" }
void f1ia_f (int a[]) { (void)&a; }
void f1ia_f (int[f (3)]);     // { dg-warning "argument 1 of type 'int\\\[f *\\\(3\\\)]' declared as a variable length array" }
void f1ia_f (int[f (4)]);     // { dg-warning "argument 1 of type 'int\\\[f *\\\(4\\\)]' declared as a variable length array" }
void f1ia_f (int[]);

void f1iaf0_f1 (int[f (0)]);  // { dg-message "previously declared as 'int\\\[f *\\\(0\\\)]'" }
void f1iaf0_f1 (int[f (1)]);  // { dg-warning "argument 1 of type 'int\\\[f *\\\(1\\\)]' declared with mismatched bound" }
void f1iaf0_f1 (int[f (0)]);
void f1iaf0_f1 (int[f (1)]);  // { dg-warning "argument 1 of type 'int\\\[f *\\\(1\\\)]' declared with mismatched bound" }
void f1iaf0_f1 (int a[f (0)]) { (void)&a; }
void f1iaf0_f1 (int[f (1)]);  // { dg-warning "argument 1 of type 'int\\\[f *\\\(1\\\)]' declared with mismatched bound" }
void f1iaf0_f1 (int[f (0)]);

void f1la_ (long[]);         // { dg-message "previously declared as an ordinary array 'long int\\\[]'" }
void f1la_ (long[nelts]);    // { dg-warning "argument 1 of type 'long int\\\[nelts]' declared as a variable length array" }
void f1la_ (long[]);
void f1la_ (long a[nelts])   // { dg-warning "argument 1 of type 'long int\\\[nelts]' declared as a variable length array" }
{ (void)&a; }
void f1la_ (long[]);

void f2ca_ (int, char[]);     // { dg-message "previously declared as an ordinary array 'char\\\[]'" }
void f2ca_ (int n, char[n]);  // { dg-warning "argument 2 of type 'char\\\[n]' declared as a variable length array" }
void f2ca_ (int, char[]);
void f2ca_ (int n, char a[n]) // { dg-warning "argument 2 of type 'char\\\[n]' declared as a variable length array" }
{ (void)&n; (void)&a; }

void f2ia1_f (int n, int[n]);     // { dg-message "previously declared as 'int\\\[n]' with bound argument 1" }
void f2ia1_f (int,   int[f (0)]); // { dg-warning "argument 2 of type 'int\\\[f *\\\(0\\\)]' declared with mismatched bound 'f *\\\(0\\\)'" }
void f2ia1_f (int m, int[m]);
void f2ia1_f (int,   int[f (1)]); // { dg-warning "argument 2 of type 'int\\\[f *\\\(1\\\)]' declared with mismatched bound 'f *\\\(1\\\)'" }
void f2ia1_f (int x, int a[x]) { (void)&x; (void)&a; }
void f2ia1_f (int,   int[f (2)]);   // { dg-warning "argument 2 of type 'int\\\[f *\\\(2\\\)]' declared with mismatched bound 'f *\\\(2\\\)'" }
void f2ia1_f (int y, int[y]);

void f2iaf_1 (int,   int[f (0)]); // { dg-message "previously declared as 'int\\\[f *\\\(0\\\)]'" }
void f2iaf_1 (int n, int[n]);     // { dg-warning "argument 2 of type 'int\\\[n]' declared with mismatched bound argument 1" }
void f2iaf_1 (int,   int[f (0)]);
void f2iaf_1 (int m, int[m]);     // { dg-warning "argument 2 of type 'int\\\[m]' declared with mismatched bound argument 1" }
void f2iaf_1 (int x, int a[f (0)]) { (void)&x; (void)&a; }
void f2iaf_1 (int y, int[y]);     // { dg-warning "argument 2 of type 'int\\\[y]' declared with mismatched bound argument 1" }


void f3ia1 (int n, int, int[n]);  // { dg-message "previously declared as 'int\\\[n]' with bound argument 1" }
void f3ia1 (int, int n, int[n]);  // { dg-warning "argument 3 of type 'int\\\[n]' declared with mismatched bound argument 2" }
void f3ia1 (int n, int, int[n]);


extern int g (int);

void f1iaf_g (int[f (1)]);    // { dg-message "previously declared as 'int\\\[f *\\\(1\\\)]'" }
void f1iaf_g (int[g (1)]);    // { dg-warning "argument 1 of type 'int\\\[g *\\\(1\\\)]' declared with mismatched bound" }
void f1iaf_g (int[f (1)]);


void nrf1iaf_g (int[f (1)]);  // { dg-message "previously declared as 'int\\\[f *\\\(1\\\)]'" }
__attribute__ ((nonnull))
void nrf1iaf_g (int[g (1)]);  // { dg-warning "argument 1 of type 'int\\\[g *\\\(1\\\)]' declared with mismatched bound" }
__attribute__ ((noreturn))
void nrf1iaf_g (int[f (1)]);
