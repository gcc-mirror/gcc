//PR c++/27820

void foo()
{
    L: L: ; // { dg-error "duplicate label" }
}

