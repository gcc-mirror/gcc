/* dg-do compile */
/* dg-options "-O3" */
typedef struct x x;
extern void *baz(char *);
struct x { char * (*bar) (int); };
static x **foo() { return ((x**)baz(0)); }
int xyzzy()
{
    baz((*foo())->bar(0));
    return 3;
}
