/* PR82337: SLSR needs to prevent abnormal SSA names from
   serving as a basis. */
char *a, *b, *c;

struct d {
  short e;
  char f[];
};

extern void j (void);

void
g() {
  struct d *h;
  char *i;
  int d;
  do {
    i = h->f + d;
    20 ? j() : 0;
    i = c;
    if (__builtin_setjmp (h))
      b = h->f + d;
    d = (int)(*i);
  } while (a);
}
