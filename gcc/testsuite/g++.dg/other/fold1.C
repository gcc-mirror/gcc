// PR middle-end/27384
// { dg-do compile }

struct A
{
    static const int i = i;  // { dg-error "not declared" }
    int x[i];		     // { dg-error "11:size of array .x. is not an integral constant-expression" }
};
