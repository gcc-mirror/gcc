// { dg-options "-w" }

template<typename T> void foo(T); // { dg-message "candidate" }
 
void bar()
{
  int i;
  int A[i][i]; 
  foo(A); // { dg-error "" } 
}
