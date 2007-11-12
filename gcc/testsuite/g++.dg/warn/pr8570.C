// PR c++/8570
// { dg-do compile }
// { dg-options "" }
template <typename T, typename P>
class X { // { dg-warning "note: previous declaration .* used 2" }
public:
  X() { }

private:
  template <typename U> friend class X; // { dg-error "error: .*redeclared with 1 template parameter" }
};

X<int, int> i;
