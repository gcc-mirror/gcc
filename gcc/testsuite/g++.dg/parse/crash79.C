// PR c++/117129
// { dg-do "compile" { target c++11 } }

struct Noncopyable {
  Noncopyable();
  Noncopyable(const Noncopyable &) = delete; // { dg-note "declared here" }
  virtual ~Noncopyable();
};
Noncopyable nrvo() { 
  {
    Noncopyable A;
    return A; // { dg-error "use of deleted function" }
	      // { dg-note "display considered" "" { target *-*-* } .-1 }
  }
}
