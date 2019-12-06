// PR c++/92271 - make __is_same alias for __is_same_as.
// { dg-do compile { target c++11 } }

static_assert(__is_same(int, int) == __is_same_as(int, int), "");
static_assert(__is_same(unsigned int, int) == __is_same_as(unsigned int, int), "");
