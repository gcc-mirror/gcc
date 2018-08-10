/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

union U
{
    int f0;
    unsigned char f1;
};

int main()
{
    int i = 0;
    union U u = {0};  /* count(1) */
    for (u.f1 = 0; u.f1 != -2; ++u.f1) {
        i ^= u.f1;  /* count(1) */
        if (i < 1)  /* count(1) */
            return 0;  /* count(1) */
    }

    return 1;
}

/* { dg-final { run-gcov gcov-pr83813.c } } */
