// { dg-do compile }
// { dg-options "-gcodeview" }

// get_type_num_struct must emit complete definitions for the unnamed types
// of S::a and S::b, without additional anonymous forward references.

struct S
{
  struct { int x; } a;
  struct { double y; } b;
};

S s;

// { dg-final { scan-assembler-times "<unnamed-tag>" 2 } }
