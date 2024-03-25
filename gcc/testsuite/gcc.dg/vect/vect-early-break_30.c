/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int x[100];
int choose1(int);
int choose2();
void consume(int);
void f() {
    for (int i = 0; i < 100; ++i) {
        if (x[i] == 11) {
            if (choose1(i))
                goto A;
            else
                goto B;
        }
    }
    if (choose2())
        goto B;
A:
    for (int i = 0; i < 100; ++i)
        consume(i);
B:
    for (int i = 0; i < 100; ++i)
        consume(i * i);
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */