/* PR tree-optimization/18947 */
extern __inline void f1 (void) { }
extern __inline void f2 (void) { f1 (); }
void f2 (void) {}
