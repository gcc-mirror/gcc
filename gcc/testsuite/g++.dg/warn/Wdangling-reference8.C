// PR c++/107532
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

struct Plane { unsigned int bytesused; };

// Passes a reference through. Does not change lifetime.
template <typename T>
struct Ref {
    const T& i_;
    Ref(const T & i) : i_(i) {}
    const T & inner();
};

struct FrameMetadata {
    Ref<const Plane> planes() const { return p_; }

    Plane p_;
};

void bar(const Plane & meta);
void foo(const FrameMetadata & fm)
{
    const Plane & meta = fm.planes().inner();
    bar(meta);
    const Plane & meta2 = FrameMetadata().planes().inner(); // { dg-warning "dangling reference" }
    bar(meta2);
}

struct S {
  const S& self () { return *this; }
} s;

const S& r1 = s.self();
const S& r2 = S().self(); // { dg-warning "dangling reference" }

struct D {
};

struct C {
  D d;
  Ref<const D> get() const { return d; }
};

struct B {
  C c;
  const C& get() const { return c; }
  B();
};

struct A {
  B b;
  const B& get() const { return b; }
};

void
g (const A& a)
{
  const auto& d1 = a.get().get().get().inner();
  (void) d1;
  const auto& d2 = A().get().get().get().inner(); // { dg-warning "dangling reference" }
  (void) d2;
  const auto& d3 = A().b.get().get().inner(); // { dg-warning "dangling reference" }
  (void) d3;
  const auto& d4 = a.b.get().get().inner();
  (void) d4;
  const auto& d5 = a.b.c.get().inner();
  (void) d5;
  const auto& d6 = A().b.c.get().inner(); // { dg-warning "dangling reference" }
  (void) d6;
  Plane p;
  Ref<Plane> r(p);
  const auto& d7 = r.inner();
  (void) d7;
  const auto& d8 = Ref<Plane>(p).inner();
  (void) d8;
}
