/* PR middle-end/19874 */
typedef enum { A, B, C, D } E;

struct S {
  E __attribute__ ((mode (__byte__))) a;
  E __attribute__ ((mode (__byte__))) b;
  E __attribute__ ((mode (__byte__))) c;
  E __attribute__ ((mode (__byte__))) d;
};

extern void abort (void);
extern void exit (int);

E
foo (struct S *s)
{
  if (s->a != s->b)
    abort ();
  if (s->c != C)
    abort ();
  return s->d;
}

int
main (void)
{
  struct S s[2];
  s[0].a = B;
  s[0].b = B;
  s[0].c = C;
  s[0].d = D;
  s[1].a = D;
  s[1].b = C;
  s[1].c = B;
  s[1].d = A;
  if (foo (s) != D)
    abort ();
  exit (0);
}

