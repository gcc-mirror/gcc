/* { dg-xfail-if "PR middle-end/17529" { "*-*-*" } { "*" } { "" } } */

static inline void 
bar (const int * const x) 
{ 
  __asm__ __volatile__ (""::"m" (*x)); 
} 
 
static const int y[]; 
 
void 
foo (void) 
{ 
  bar (y); 
} 
