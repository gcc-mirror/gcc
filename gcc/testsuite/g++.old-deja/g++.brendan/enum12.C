// { dg-do assemble  }
// GROUPS passed enums
enum Bool { False, True };

enum Bool object;

struct S
{
  Bool field:1;

  void copy_enum_bit_field () const { object = field; }
};
