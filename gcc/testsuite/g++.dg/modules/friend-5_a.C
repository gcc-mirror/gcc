// { dg-additional-options -fmodules-ts }
// From Andrew Sutton

export module foo;
// { dg-module-cmi foo }
export class A {
  friend class B;
};
