// { dg-options "-w" }

template<typename T> void foo(T); // { dg-message "note" }
 
void bar()
{
  int i;
  int A[i][i]; 
  foo(A); // { dg-error "" } 
  // { dg-message "(candidate|not a valid template argument)" "candidate note" { target *-*-* } .-1 }
}
