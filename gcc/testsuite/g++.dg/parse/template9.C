template <typename T> 
void f() {
  g(); // { dg-error "" }
  h(3); // { dg-error "" }
}
