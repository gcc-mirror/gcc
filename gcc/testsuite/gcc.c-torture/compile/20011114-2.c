typedef struct { int c, d, e, f, g; } D;

void bar (unsigned long, unsigned long);
void foo (D *y)
{
  int x = 0;

  if (y->f == 0)
    x |= 0x1;
  if (y->g == 0)
    x |= 0x2;
  bar ((x << 16) | (y->c & 0xffff), (y->d << 16) | (y->e & 0xffff));
}
