/* { dg-lto-do run } */
/* { dg-lto-options { { -O2 -flto } } } */

static char false_object;
static char true_object;

__attribute__((noinline))
static
char *less_equal(long comparison) {
    if (comparison == 0) goto return_true;
    if (comparison < 0) goto return_true;
    goto return_false;

return_true:
    return &true_object;

return_false:
    return &false_object;
}

__attribute__((noinline))
static
char *greater_equal(long comparison) {
    if (comparison == 0) goto return_true;
    if (comparison < 0) goto return_false;
    goto return_true;

return_true:
    return &true_object;

return_false:
    return &false_object;
}

int main(void) {
    volatile long negative = -1;

    int correct =
        less_equal(negative) == &true_object &&
        greater_equal(negative) == &false_object;

    if (!correct)
      __builtin_abort ();
    return 0;
}
