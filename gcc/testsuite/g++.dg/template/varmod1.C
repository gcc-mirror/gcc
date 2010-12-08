// { dg-options "-w" }

template<typename T> void foo(T); // { dg-message "note" }
 
void bar()
{
  int i;
  int A[i][i]; 
  foo(A); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 9 }
}
