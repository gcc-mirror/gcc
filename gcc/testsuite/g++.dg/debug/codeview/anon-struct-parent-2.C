// { dg-do compile }
// { dg-options "-gcodeview" }

// get_type_num_struct must not re-enter an unfinished anonymous parent.

struct S
{
  struct
  {
    struct { int a; } b;
  } c;
} s;

// { dg-final { scan-assembler-times "<unnamed-tag>" 2 } }
