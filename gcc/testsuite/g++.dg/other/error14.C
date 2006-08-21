//PR c++/26269

void foo()
{
    i;          // { dg-error "not declared in this scope" }
    int i;
}
