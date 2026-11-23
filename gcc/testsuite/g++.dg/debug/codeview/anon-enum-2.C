// { dg-do compile }
// { dg-options "-std=c++11 -O0 -gcodeview" }

// write_lf_enum must emit a name for an anonymous enum used directly.

enum
{
  value
} object;

int
f ()
{
  return object == value;
}
