// PR c++/12226

class foo {
private:
  foo(const foo &);
public:
  foo();
};
const foo &bar = foo();

class derived : public foo {
private:
  derived(const derived&);
public:
  derived();
};

const foo& baz = derived();
