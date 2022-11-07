/* { dg-do compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -flive-patching=inline-only-static -flto" } */

int main()
{
  return 0;
}

/* { dg-message "sorry, unimplemented: live patching \\(with 'inline-only-static'\\) is not supported with LTO" "-flive-patching and -flto together" { target *-*-* } 0 } */
