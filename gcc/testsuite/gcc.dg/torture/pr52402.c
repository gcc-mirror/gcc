/* { dg-do run } */
/* { dg-options "-w -Wno-psabi" } */
/* { dg-require-effective-target int32plus } */
/* { dg-xfail-run-if "pr52450" { { hppa*-*-hpux* } && { ! lp64 } } } */

typedef int v4si __attribute__((vector_size(16)));
struct T { v4si i[2]; int j; } __attribute__((packed));

static v4si __attribute__((noinline))
foo (struct T t)
{
  return t.i[0];
}

static struct T *__attribute__((noinline))
init ()
{
  char *p = __builtin_malloc (sizeof (struct T) + 1);
  p++;
  __builtin_memset (p, 1, sizeof (struct T));
  return (struct T *)p;
}

int main()
{
  struct T *p;
  p = init ();
  if (foo (*p)[0] != 0x01010101)
    __builtin_abort ();
  return 0;
}
