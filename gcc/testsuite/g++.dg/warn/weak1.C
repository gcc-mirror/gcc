// { dg-do run }
// { dg-do compile { target *-*-coff i?86-pc-cygwin } }
// { dg-warning "weak declaration" "COFF format does not support weak" { target *-*-coff i?86-pc-cygwin } 5 }

extern void foo (void) __attribute__ ((weak));

int
main ()
{
  if (&foo)
    foo ();

  return 0;
}
