// { dg-options "-Wignored-qualifiers" }
struct Test {
   operator int const(); // { dg-warning "type qualifiers ignored" }
   operator int const() const; // { dg-warning "type qualifiers ignored" }
};
