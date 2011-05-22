/* PR middle-end/49029 */
struct S { volatile unsigned f : 11; signed g : 30; } __attribute__((packed));
struct T { volatile struct S h; } __attribute__((packed)) a;
void foo (int);

void
bar ()
{
  foo (a.h.g);
}
