// PR c++/22464

template<typename T>
void do_something(const T* A) { // { dg-error "declared" }
  struct helper_t { 
    helper_t() {  
      A[0]; // { dg-error "use" }
    }
  } helper;
}

void sub1() {
  double A[7];
  do_something (A);
}
