/* PR optimization/12260.  */

extern int f(void);
extern int g(int);

static char buf[512];
void h(int l) {
    while (l) {
        char *op = buf;
        if (f() == 0)
            break;
        if (g(op - buf + 1))
            break;
    }
}
