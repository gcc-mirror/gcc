// PR c++/64667
// { dg-options "-Winit-self" }

class A
{
public:
  A(const A&) : a(a) {}  // { dg-warning "initialized with itself" }
private:
  int a;
};

class B
{
public:
  B(const B&) : b(b) {}  // { dg-warning "initialized with itself" }
private:
  int* b;
};

class C
{
public:
  C(const C&) : c(c) {}  // { dg-warning "initialized with itself" }
private:
  int& c;
};
