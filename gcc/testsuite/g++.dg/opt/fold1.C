// PR middle-end/13696
// { dg-do compile }
// { dg-options "-O2" }

extern void x(unsigned long*);

enum e { red, blue, green };

struct s {
  unsigned long l;
};
struct s map[1][256];

void
f(int i,e j) {
     x(&(map[i][j].l));
}
