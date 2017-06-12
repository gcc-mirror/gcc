template <typename T> 
void f() {
  g(); // { dg-error "must be available" "err" }
       // { dg-message "note" "note" { target *-*-* } .-1 }
  h(3); // { dg-error "must be available" }
}
