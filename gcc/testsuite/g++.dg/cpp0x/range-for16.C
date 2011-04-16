// Test for range-based for loop with arrays of
// incomplete type or unknown size

// { dg-do compile }
// { dg-options "-std=c++0x" }

extern int a[10];
extern int b[];

struct S;
extern S c[10];
extern S d[];

void test()
{
    for (int n : a);
    for (int n : b); // { dg-error "incomplete type" }
    for (S &n : c); // { dg-error "incomplete type" }
    for (S &n : d); // { dg-error "incomplete type" }
    for (int n : *c); // { dg-error "incomplete type" }
}
