// PR c++/79501
// { dg-do compile { target c++17 } }

struct X {
protected:
  template<class T>
  struct B { T t; };

  template<class T> B(T) -> B<T>;
};

struct Y {
protected:
  template<class T>
  struct B { T t; };

private:
  template<class T> B(T) -> B<T>; // { dg-error "access" }
};
