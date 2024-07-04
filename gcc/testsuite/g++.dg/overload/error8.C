// PR c++/111160
// { dg-do compile { target c++11 } }

class TheClass {}; // { dg-error "discards|bind|discards|bind" }
void the_func() {
  TheClass x;
  volatile TheClass y;
  (false ? x : x) = y; // { dg-error "ambiguous|ambiguous" }
}
