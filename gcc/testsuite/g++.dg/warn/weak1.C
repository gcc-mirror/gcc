// { dg-do run }
// { dg-do compile { target *-*-coff } }
// { dg-warning "weak declaration" "COFF format does not support weak" { target *-*-coff } 5 }

extern void foo (void) __attribute__ ((weak));

int
main ()
{
  if (&foo)
    foo ();

  return 0;
}
