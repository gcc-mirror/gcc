// PR c++/111840
// { dg-do compile { target c++11 } }

int f1() = delete("should have a reason"); // { dg-error "'delete' reason only available with" "" { target c++23_down } }
int f2() = delete[""]; // { dg-error "expected" }
int f3() = delete{""}; // { dg-error "expected" }
int f4() = delete""; // { dg-error "expected" }
int f5() = delete[{'a'""; // { dg-error "expected" }
int i = f5();
