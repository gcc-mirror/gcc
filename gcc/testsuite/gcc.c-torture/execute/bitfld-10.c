/* PR tree-optimization/102622 */
/* Wrong code introduced due to phi-opt
   introducing undefined signed interger overflow
   with one bit signed integer negation. */

struct f{signed t:1;};
int g(struct f *a, int t) __attribute__((noipa));
int g(struct f *a, int t)
{
    if (t)
      a->t = -1;
    else
      a->t = 0;
    int t1 = a->t;
    if (t1) return 1;
    return t1;
}

int main(void)
{
    struct f a;
    if (!g(&a, 1))  __builtin_abort();
    return 0;
}
