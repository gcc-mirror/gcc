/* { dg-lto-do run }  */
/* { dg-suppress-ld-options {-fPIC} }  */
/* { dg-require-effective-target tls } */
/* { dg-extra-ld-options "-pthread" { target *-*-solaris2.[89] } } */
int foo (int x)
{
  return x;
}
