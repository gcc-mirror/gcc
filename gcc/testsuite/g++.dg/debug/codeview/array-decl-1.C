// { dg-do compile }
// { dg-options "-O0 -gcodeview -femit-struct-debug-detailed=none" }

// get_type_num_array_type must terminate if the element type is represented
// by a declaration-only DIE without DW_AT_byte_size.

struct S
{
  int i;
};

int
f ()
{
  S a[2] = {};
  return a[0].i;
}
