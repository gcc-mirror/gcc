// PR c++/12226

class foo {
private:
  foo(const foo &); // { dg-error "" }
public:
  foo();
};
const foo &bar = foo(); // { dg-error "" }

class derived : public foo {
private:
  derived(const derived&);  // { dg-error "" }
public:
  derived();
};

const foo& baz = derived(); // { dg-error "" }
