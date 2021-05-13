/* PR c/100550 - ICE: in fold_convert_loc with function call VLA argument
   { dg-do compile }
   { dg-options "-Wall" } */

struct S { int i; };

extern void v;
extern void *pv;
extern struct S s;
void vf (void);

/* Verify that a function redeclaration with an invalid VLA doesn't ICE.  */

void f0 (int[]);
void f0 (int[undeclared]);    // { dg-error "undeclared" }

void f1 (int[]);
void f1 (int[-1]);            // { dg-error "size" }

void f2 (int[]);
void f2 (int[v]);             // { dg-error "size" }

void f3 (int[]);
void f3 (int b[s]);           // { dg-error "size" }

void f4 (int[]);
void f4 (int c[pv]);          // { dg-error "size" }

void f5 (int[]);
void f5 (int d[vf ()]);       // { dg-error "size" }
