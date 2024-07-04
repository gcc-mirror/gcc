// PR c++/43452

class Foo;         // { dg-message "7:forward declaration" }
int main() {
   Foo* p;         // { dg-warning "9:.p. has incomplete type" "" { target c++23_down } }
   delete [] p;    // { dg-warning "4:possible problem detected in invocation of operator .delete \\\[\\\]." "" { target c++23_down } }
   // { dg-message "4:neither the destructor nor the class-specific" "note" { target c++23_down } .-1 }
   // { dg-error "operator 'delete \\\[\\\]' used on incomplete type" "" { target c++26 } .-2 }
}
