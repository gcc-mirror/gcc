// { dg-do compile }
// { dg-options "-gcodeview" }

// get_type_num_struct must not add forward references for a declaration
// parent of nested types; only Outer * needs one.

struct Outer
{
  virtual void f ();
  struct B { int i; };
  struct C { int j; };
};

Outer::B b;
Outer::C c;
Outer *p;

// { dg-final { scan-assembler-times {\.ascii "Outer\\0"} 1 } }
