// { dg-do compile }
// { dg-options "-std=c++11 -O0 -gcodeview" }

// write_lf_enum must emit a name for an anonymous enum referenced by a
// typedef.

typedef enum
{
  value
} enum_type;

enum_type
f (enum_type x)
{
  return x;
}
