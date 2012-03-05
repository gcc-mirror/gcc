/* PR c/11446: packed on a struct takes precedence over aligned on the type
   of a field.  */
/* { dg-do run } */
/* { dg-additional-options "-mno-ms-bitfields" { target *-*-mingw* } } */

extern void abort (void);

struct A {
  double d;
} __attribute__ ((aligned));

struct B {
  char c;
  struct A a;
} __attribute__ ((packed));

int main ()
{
  if (sizeof (struct B) != sizeof (char) + sizeof (struct A))
    abort ();
  return 0;
}
