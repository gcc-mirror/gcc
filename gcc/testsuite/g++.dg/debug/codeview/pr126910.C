// PR debug/126910
// { dg-do compile }
// { dg-options "-std=c++11 -O0 -gcodeview" }

// get_type_num_enumeration_type must handle an enum with no enumerators.

enum class E {};

void
f (E)
{
}
