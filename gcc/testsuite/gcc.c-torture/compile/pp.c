/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

foo (a, b, c, d, e, i0, f, i1)
     double a, b, c, d, e, f;
     int i0, i1;
{}

main ()
{
  foo (1.0, 2.0, 3.0, 4.0, 5.0, 1, 6.0, 2);
}
