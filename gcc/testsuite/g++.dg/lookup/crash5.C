// { dg-do compile }
//
// PR 17618

void foo()
{
    p; // { dg-error "not declared" }
    (void*) p;
}
