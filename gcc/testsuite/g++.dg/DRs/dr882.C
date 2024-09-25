// DR882 - Defining main as deleted
// PR c++/116169
// { dg-do compile { target c++11 } }

int main() = delete; // { dg-error "cannot be deleted" }
