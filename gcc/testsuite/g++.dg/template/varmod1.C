// { dg-options "-w" }

template<typename T> void foo(T);
 
void bar()
{
  int i;
  int A[i][i]; 
  foo(A); // { dg-error "" } 
}
