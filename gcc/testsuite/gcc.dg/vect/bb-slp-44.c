/* { dg-do run } */

typedef struct {
    unsigned long a;
    unsigned long b;
    unsigned long c;
} data_o;
typedef struct {
    unsigned long c;
    unsigned long gap1;
    unsigned long b;
    unsigned long gap2;
    unsigned long a;
    unsigned long x;
} data_i;

volatile unsigned long gx;
void __attribute__((noipa))
bar(unsigned long x)
{
  gx = x;
}

void __attribute__((noipa))
foo(data_o *o, data_i *i)
{
  o->a = i->a;
  o->b = i->b;
  o->c = i->c;
  bar (i->x);
}

int main()
{
  unsigned long data[9];
  if ((__UINTPTR_TYPE__)data & 15 != 0)
    foo ((data_o *)&data[6], (data_i *)data);
  else
    foo ((data_o *)data, (data_i *)&data[3]);
  return 0;
}
