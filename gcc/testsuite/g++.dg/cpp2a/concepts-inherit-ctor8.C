// PR c++/94549
// { dg-do compile { target concepts } }

struct base {
  template <typename type>
    requires false
  base(type);

  template <typename type>
    requires true
  base(type);
};

struct derived : base {
  using base::base;
};

void foo() {
  derived{'G'};
}
