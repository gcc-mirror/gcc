static inline void 
bar (const int * const x) 
{ 
  __asm__ __volatile__ ("paddd" " %0, %%" "mm0"::"m" (*x)); 
} 
 
static const int y[]; 
 
void 
foo (void) 
{ 
  bar (y); 
} 
