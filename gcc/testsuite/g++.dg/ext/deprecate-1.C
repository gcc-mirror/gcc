// be pickier about anon-union and structs
// { dg-options "-fpermissive" }

struct X
{
  struct 
  {
    int f1 (); // { dg-warning "public non-static data" }
    // { dg-message "will be removed" "" { target *-*-* } .-1 }
    typedef int t1;  // { dg-warning "public non-static data" }
  private:
    int m1; // { dg-warning "public non-static data" }
  };

  union
  {
    int f2 (); // { dg-warning "public non-static data" }
    typedef int t2; // { dg-warning "public non-static data" }
  protected:
    int m2; // { dg-warning "public non-static data" }
  };
};
