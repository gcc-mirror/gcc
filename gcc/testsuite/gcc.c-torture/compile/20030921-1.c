/* PR 12281  The darwin back-end was causing the function 
   f is not being emitted. TREE_SYMBOL_REFERENCED was being set
   instead of calling mark_referenced.  */


static void f(void);
void g(void (*x) (void)){x();}
static inline void f(void){}
void h(){g(f);}
int main(){h();return 0;}
