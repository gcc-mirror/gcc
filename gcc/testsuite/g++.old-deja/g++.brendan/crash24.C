// { dg-do assemble  }
// { dg-options "-O" }
// GROUPS passed old-abort
// gcc puts the array into a register, and then the store_bit_field () code
// in expmed.c gets confused when it tries to store zero past the end of the
// register (because the index is past the array bounds).   It ends up calling
// store_split_bit_field, which then aborts, because we don't have a split bit
// field.
//
// Seems easiest to detect this case in the front end, i.e. access outside the
// array bounds, and then force the array to be allocated on the stack instead
// of a register.

int
main()
{
  char i[1];

  i[1] = 0;

  return 0;
}
