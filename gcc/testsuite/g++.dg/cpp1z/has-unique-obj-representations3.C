// PR c++/109096
// { dg-do compile { target c++11 } }

#define INTB (__SIZEOF_INT__ * __CHAR_BIT__)
struct U { int i : INTB * 3 / 4; int : INTB / 4; };
struct V { int : INTB * 3 / 4; int j : INTB / 4; };
struct W { int i; int : 0; int j; };
static_assert (__has_unique_object_representations (U) == false, "");
static_assert (__has_unique_object_representations (V) == false, "");
static_assert (sizeof (W) != 2 * sizeof (int) || __has_unique_object_representations (W) == true, "");
