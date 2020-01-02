// PR c++/43452

class Foo;         // { dg-message "7:forward declaration" }
int main() {
   Foo* p;         // { dg-warning "9:.p. has incomplete type" }
   delete [] p;    // { dg-warning "4:possible problem detected in invocation of operator .delete \\\[\\\]." }
   // { dg-message "4:neither the destructor nor the class-specific" "note" { target *-*-* } .-1 }
}
