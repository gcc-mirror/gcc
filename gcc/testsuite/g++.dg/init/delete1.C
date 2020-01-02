// PR c++/19811

class C; // { dg-message "7:forward" }

void foo(void *p) {
  delete [] ((C*)p) ; // { dg-warning "3:possible problem detected in invocation of operator .delete \\\[\\\]." }
  // { dg-message "3:neither the destructor nor the class-specific" "note" { target *-*-* } .-1 }
  // { dg-warning "invalid use of incomplete type" "" { target *-*-* } .-2 }
}
