// PR rtl-optimization/16590
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort(); 
 
struct iterator { 
  char * p; 
  int *dummy; 
}; 
 
static iterator pend(char * start) { 
  iterator p = {start, 0}; 
  if (p.p == start) p.p = start+5; 
  --p.p; 
  return p; 
} 
 
int main() { 
  char mem[4+1]; 
 
  if(pend(mem).p != mem+4) 
    abort (); 
} 
