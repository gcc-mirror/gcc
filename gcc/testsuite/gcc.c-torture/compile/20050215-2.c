/* PR tree-optimization/18947 */
/* { dg-options "-fgnu89-inline" } */
int v;
extern __inline void f1 (void) { v++; }
void f4 (void) { f1 (); }
extern __inline void f2 (void) { f1 (); }
void f3 (void) { f2 (); }
void f2 (void) { f1 (); }
