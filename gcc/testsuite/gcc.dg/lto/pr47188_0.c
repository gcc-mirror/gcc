/* { dg-lto-do assemble } */
/* { dg-require-linker-plugin "" } */
/* { dg-extra-ld-options "-fuse-linker-plugin -fwhole-program" } */
int foo(void)
{
  return 0;
}
