#include <stdio.h>

struct test {
        int one;
        int two;
};

void func2(const struct test *t)
{
        if (t->one == 0)
                printf("init func2\n");

        if (t->two == 0)  /* { dg-warning "uninitialized" } */
                printf("uninit func2\n");
}

void func1(struct test *t)
{
        t->one = 1;
        func2(t);
}

int func3(int num)
{
        if (num)
                return num;
        else
                return 0;
}

void func4(int *a, int max)
{
        int i;
        // skip the first
        for (i=1; i<max; i++)
                a[i] = 0;
}

void func5(const int *a, int max)
{
        /* a[0] is uninitialized, but the rest of the array is initialized.  */
        int i;
        for (i=0; i<max; i++) {
                if (a[i]) /* { dg-warning "uninitialized" "" { xfail *-*-* } } */
                        printf("func5: %d\n", i);
        }
}

int func6(const int *num)
{
        if (*num)  /* { dg-warning "uninitialized" } */
                return *num;
        else
                return 0;
}

int j;
int func7(void)
{
        return j;  /* { dg-bogus "uninitialized" } */
}

void func8(const int *a, int max)
{
        int i;
        for (i=0; i<max; i++) {
                if (a[i]) /* { dg-warning "uninitialized" } */
                        printf("func8: %d\n", i);
        }
}

enum {RED, AMBER, GREEN, BLACK};

int test_1 (void)
{
        struct test t;  /* { dg-message "region created on stack here" } */

        func1(&t);
        return 0;
}

int test_2 (void)
{
        int num;  /* { dg-message "region created on stack here" } */

        func3(num);  /* { dg-warning "use of uninitialized value 'num'" } */
        return 0;
}

int test_3 (void)
{
        int arry[10];

        func4(arry, 10);
        func5(arry, 10);

        return 0;
}

int test_4 (void)
{
        int num;  /* { dg-message "region created on stack here" } */

        func6(&num);
        return 0;
}

int test_5 (void)
{
        int arry_2[10];  /* { dg-message "region created on stack here" } */

        printf("func7: %d\n", func7());
        func8(arry_2, 10);

        return 0;
}

int test_6 (void)
{
        int go;  /* { dg-message "region created on stack here" } */
        int color = BLACK;

        switch (color) {
        case RED:
        case AMBER:
                go = 0;
                break;
        case GREEN:
                go = 1;
                break;
        }

        printf("go :%d\n", go); /* { dg-warning "use of uninitialized value 'go'" } */

        return 0;
}
