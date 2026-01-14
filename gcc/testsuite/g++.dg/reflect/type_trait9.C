// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], type property
// queries.

#include <meta>
using namespace std::meta;

class ClassType { };

static_assert (rank (^^int) == 0);
static_assert (rank (^^const void) == 0);
static_assert (rank (^^char &) == 0);
static_assert (rank (^^long &&) == 0);
static_assert (rank (^^int [2]) == 1);
static_assert (rank (^^int [][4]) == 2);
static_assert (rank (^^int [2][2][4][4][6][6]) == 6);
static_assert (rank (^^int []) == 1);
static_assert (rank (^^ClassType) == 0);
static_assert (rank (^^ClassType [2]) == 1);
static_assert (rank (^^ClassType [][4]) == 2);
static_assert (rank (^^ClassType [2][2][4][4][6][6]) == 6);
static_assert (rank (^^ClassType []) == 1);
using D = ClassType [2][2][4][4][6][6][6];
static_assert (rank (^^D) == 7);

static_assert (extent (^^int) == 0);
static_assert (extent (^^const void, 0) == 0);
static_assert (extent (^^char &, 42) == 0);
static_assert (extent (^^long &&, 18) == 0);
static_assert (extent (^^int [2]) == 2);
static_assert (extent (^^int [2][4]) == 2);
static_assert (extent (^^int [][4]) == 0);
static_assert (extent (^^int []) == 0);
static_assert (extent (^^int, 0) == 0);
static_assert (extent (^^int [2], 0) == 2);
static_assert (extent (^^int [2][4], 0) == 2);
static_assert (extent (^^int [][4], 0) == 0);
static_assert (extent (^^int [], 0) == 0);
static_assert (extent (^^int, 1) == 0);
static_assert (extent (^^int [2], 1) == 0);
static_assert (extent (^^int [2][4], 1) == 4);
static_assert (extent (^^int [][4], 1) == 4);
static_assert (extent (^^int [10][4][6][8][12][2], 4) == 12);
static_assert (extent (^^int [], 1) == 0);
static_assert (extent (^^ClassType) == 0);
static_assert (extent (^^ClassType [2]) == 2);
static_assert (extent (^^ClassType [2][4]) == 2);
static_assert (extent (^^ClassType [][4]) == 0);
static_assert (extent (^^ClassType, 0) == 0);
static_assert (extent (^^ClassType [2], 0) == 2);
static_assert (extent (^^ClassType [2][4], 0) == 2);
static_assert (extent (^^ClassType [][4], 0) == 0);
static_assert (extent (^^ClassType, 1) == 0);
static_assert (extent (^^ClassType [2], 1) == 0);
static_assert (extent (^^ClassType [2][4], 1) == 4);
static_assert (extent (^^ClassType [][4], 1) == 4);
static_assert (extent (^^ClassType [10][4][6][8][12][2], 4) == 12);

__extension__ void
ext ()
{
  static_assert (rank (^^int [0]) == 1);
  static_assert (extent (^^int [0]) == 0);
  static_assert (extent (^^int [0], 0) == 0);
  static_assert (extent (^^int [0], 1) == 0);
}
