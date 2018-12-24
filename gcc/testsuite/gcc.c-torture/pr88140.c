typedef struct {
} a;

typedef struct {
  a *b[0];
} c;

void d() { ((c *)0)->b[0] = 0; }
