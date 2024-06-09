/* { dg-options "-fcondition-coverage -ftest-coverage -fprofile-update=atomic" } */
/* { dg-do run { target native } } */
/* { dg-require-effective-target profile_update_atomic } */

/* Some side effect to stop branches from being pruned */
int x = 0;

void
conditions_atomic001 (int a, int b)
{
    if (a || b) /* conditions(1/4) true(0) false(0 1) */
		/* conditions(end) */
	x = 1;
    else
	x = 2;
}

int main ()
{
    conditions_atomic001 (0, 1);
}

/* { dg-final { run-gcov conditions { --conditions gcov-20.c } } } */
