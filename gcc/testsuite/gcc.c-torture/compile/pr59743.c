/* PR middle-end/59743 */

typedef union {
  long all;
  struct {
    int low;
    int high;
  } s;
} udwords;
int a, b, c, d;
void __udivmoddi4() {
  udwords r;
  d = __builtin_clz(0);
  r.s.low = 0;
  for (; d; --d) {
    r.s.high = r.s.high << 1 | r.s.low >> a;
    r.s.low = r.s.low << b >> 1;
    int s = -r.all;
    c = s;
    r.all--;
  }
}

