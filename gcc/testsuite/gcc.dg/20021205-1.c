/* dg-do compile */
/* dg-options "-O3" */
typedef struct x x;
struct x { char * (*bar) (int); };
static x **foo() { return ((x**)baz()); }
int xyzzy()
{
    baz((*foo())->bar(0));
    return 3;
}
