/* { dg-additional-options "-O1" } */

struct List {
    struct List *next;
};

void foo(struct List *p, struct List *q)
{
    while (p && p != q){
        p = p->next;
    }
}

int main()
{
    struct List x = {0};
    foo(0, &x);
    return 0;
}

