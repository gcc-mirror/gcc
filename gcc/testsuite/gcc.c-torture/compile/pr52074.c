/* PR middle-end/52074 */

struct S { const char *d, *e; } __attribute__((packed));

void
foo (const char **p, struct S *q)
{
  *p = "abcdef";
  q->d = "ghijk";
}
