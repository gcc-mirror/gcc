
static inline void 
bar (const int * const x) 
{ 
  __asm__ __volatile__ (""::"m" (*x)); 
} 
 
static const int y[1]; 
 
void 
foo (void) 
{ 
  bar (y); 
} 
