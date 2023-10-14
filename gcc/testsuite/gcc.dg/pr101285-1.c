/* { dg-do compile } */
/* { dg-options "-W -Wall" } */
const int b;
typedef void (*ft1)(int[b++]); /* { dg-error "read-only variable" } */
void bar(int * z);
void baz()
{
    (ft1) bar; /* { dg-warning "statement with no effect" } */
}

