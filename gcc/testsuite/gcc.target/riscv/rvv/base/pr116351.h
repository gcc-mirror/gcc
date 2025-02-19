#ifndef PR116351_H
#define PR116351_H

#define T long

int a, b, c;
short d, e, f;

T g (T h) { return h; }

void i () {
  for (; b; ++b) {
    f = 5 >> a ? d : d << a;
    e &= c | g(f);
  }
}

#endif
