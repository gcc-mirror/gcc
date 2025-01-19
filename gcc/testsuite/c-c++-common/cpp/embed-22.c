/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-std=c23" { target c } } */

typedef unsigned char V __attribute__((vector_size (128)));

V a;

void
foo (void)
{
  V b = {
    #embed __FILE__ limit (128) gnu::offset (3)
  };
  a = b;
}

const unsigned char c[] = {
  #embed __FILE__ limit (128) gnu::offset (3)
};

int
main ()
{
  foo ();
  if (__builtin_memcmp (&c[0], &a, sizeof (a)))
    __builtin_abort ();
}
