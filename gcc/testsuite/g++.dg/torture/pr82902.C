// { dg-do compile }

typedef struct el_t {
    el_t *next;
    int elem[];
} EL;
el_t a, c;
void *b;
void *fn1() {
    if (b)
      return a.elem;
    return c.elem;
}
typedef struct {
    int x;
} EV_T;
EV_T *d;
void fn2() {
    EV_T *e = (EV_T *)fn1();
    d[0] = *e;
}
