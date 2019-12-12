
/* { dg-options "-fprofile-arcs -ftest-coverage -std=c++11" } */
/* { dg-do run { target native } } */

int main()
{
    auto partially_uncovered_lambda = [](int i) { /* count(1) */
        if (i > 10) /* count(1) */
            return 0; /* count(1) */
        return 1; /* count(#####) */
    };

    return partially_uncovered_lambda(20); /* count(1) */
}

/* { dg-final { run-gcov pr86109.C } } */
