// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

class ClassType { };

SA(__is_same(__remove_all_extents(int), int));
SA(__is_same(__remove_all_extents(int[2]), int));
SA(__is_same(__remove_all_extents(int[2][3]), int));
SA(__is_same(__remove_all_extents(int[][3]), int));
SA(__is_same(__remove_all_extents(const int[2][3]), const int));
SA(__is_same(__remove_all_extents(ClassType), ClassType));
SA(__is_same(__remove_all_extents(ClassType[2]), ClassType));
SA(__is_same(__remove_all_extents(ClassType[2][3]), ClassType));
SA(__is_same(__remove_all_extents(ClassType[][3]), ClassType));
SA(__is_same(__remove_all_extents(const ClassType[2][3]), const ClassType));
