/* PR tree-optimization/18947 */
int v;
extern __inline void f0 (void) { v++; }
extern __inline void f1 (void) { f0 (); }
void f4 (void) { f1 (); }
extern __inline void f2 (void) { f1 (); }
void f3 (void) { f2 (); }
void f2 (void) { f1 (); }
