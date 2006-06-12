// PR c++/27951
// { dg-do compile }

void foo()
{
    int i;             // { dg-error "previously" }
    union { int i; };  // { dg-error "redeclaration" }
}
