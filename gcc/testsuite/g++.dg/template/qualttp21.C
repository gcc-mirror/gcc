// Copyright (C) 2002 Free Software Foundation
// Contributed by Roger Sayle <roger@eyesopen.com>
// { dg-do compile }

template <class A>
class foo {
   int _foo;
public:
   foo() {}
protected:
   ~foo() {} // { dg-message "protected" }
};

int main()
{
  foo<int> a; // { dg-error "context" }
}
