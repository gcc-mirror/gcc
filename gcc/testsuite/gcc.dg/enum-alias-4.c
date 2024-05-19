/* { dg-do run } */
/* { dg-options "-O2" } */

typedef int A;
typedef int __attribute__ (( hardbool(0, 1) )) B;

_Static_assert(_Generic((A*){ 0 }, B*: 1), "");

void* foo(void* a, void *b, A *c, B *d)
{
        *(A**)a = c;
        *(B**)b = d;
        return *(A**)a;
}

int main()
{
        A *a, b, c;
        if (&c != (A*)foo(&a, &a, &b, &c))
                __builtin_abort();
}

