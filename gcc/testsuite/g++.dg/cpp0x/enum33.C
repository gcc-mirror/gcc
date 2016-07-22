// PR c++/70241
// { dg-do compile { target c++11 } }

class A {
public:
  enum B : int;
  enum class C : int;
private:
  enum B : int { }; // { dg-error "different access" }
  enum class C : int { }; // { dg-error "different access" }
};
