// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

class ClassType { };

SA(__is_same(__remove_extent(int), int));
SA(__is_same(__remove_extent(int[2]), int));
SA(__is_same(__remove_extent(int[2][3]), int[3]));
SA(__is_same(__remove_extent(int[][3]), int[3]));
SA(__is_same(__remove_extent(const int[2]), const int));
SA(__is_same(__remove_extent(ClassType), ClassType));
SA(__is_same(__remove_extent(ClassType[2]), ClassType));
SA(__is_same(__remove_extent(ClassType[2][3]), ClassType[3]));
SA(__is_same(__remove_extent(ClassType[][3]), ClassType[3]));
SA(__is_same(__remove_extent(const ClassType[2]), const ClassType));
