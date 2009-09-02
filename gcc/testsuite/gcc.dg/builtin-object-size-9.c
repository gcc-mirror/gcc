/* { dg-do run } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;
extern void *malloc (size_t);
extern void free (void *);
extern void abort (void);

union U
{
  struct S { int a; int b; } s;
  int t;
};

struct T
{
  int c;
  char d[1];
};

int
main (void)
{
  union U *u = malloc (sizeof (struct S) + sizeof (struct T) + 6);
  struct T *t = (struct T *) (&u->s + 1);
  if (__builtin_object_size (t->d, 1)
      != sizeof (struct T) + 6 - __builtin_offsetof (struct T, d))
    abort ();
  free (u);
  return 0;
}
