// PR c++/95288

void f()
{
  enum X 
    { // { dg-message "to match this" }
      a. // { dg-error "expected" }
      b
    }; // { dg-error "extra" "" { target c++98_only } }
} // { dg-error "expected" }
