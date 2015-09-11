/* Check that __do_global_ctors_aux can be reached from .init section that
   is in a different (256MB) region. */
/* { dg-do run { target { "mips*-*-linux*" } } } */
/* { dg-skip-if "" { "mips*-sde-elf mips*-mti-elf mips*-img-elf" } } */
/* { dg-options "-Wl,--section-start=.init=0x0FFF0000" } */
/* { dg-options "-Wl,--section-start=.text=0x10000000" } */
/* { dg-options "-mips32r2" } */

int
main (void) {
  return 0;
}
