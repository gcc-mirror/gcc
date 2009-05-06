typedef struct {
    int *p;
} *A;

extern const int a[1];
extern const int b[1];

void foo()
{
  A x;
  A y;
  static const int * const c[] = { b };

  x->p = (int*)c[0];
  y->p = (int*)a;
}

