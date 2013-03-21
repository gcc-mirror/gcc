// PR c++/55240
// { dg-do compile { target c++11 } }

int main()
{
    int q = 1;				 // { dg-error "declared here" }
    struct test { int x = q; } instance; // { dg-error "local variable" }
}
