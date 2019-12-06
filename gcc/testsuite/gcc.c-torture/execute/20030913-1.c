/* Assignments via pointers pointing to global variables were being killed
   by SSA-DCE.  Test contributed by Paul Brook <paul@nowt.org>  */

int g;
 
void 
fn2(int ** q) 
{ 
  *q = &g;
} 
 
void test() 
{ 
  int *p; 
 
  fn2(&p); 
 
  *p=42; 
} 
 
int main() 
{ 
  test(); 
  if (g != 42) abort();
  exit (0); 
}
