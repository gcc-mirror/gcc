/* { dg-lto-do link } */
/* { dg-require-effective-target elf } */
/* { dg-require-effective-target named_sections } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target shared } */
/* { dg-require-effective-target strub } */
/* { dg-require-linker-plugin "" } */
/* { dg-lto-options { { -O2 -flto -fPIC -shared -ffunction-sections -save-temps -fstrub=all -fno-ipa-icf } } } */

static int
same_fn (int x)
{
  return x * 33 + 7;
}

int (*a_callback) (int) = same_fn;

/* The strub wrappers must use sections derived from their privatized
   assembler names.  */
/* { dg-final { scan-lto-assembler {\.section[ \t]+\.text\.same_fn\.lto_priv\.0[, \t"]} } } */
/* { dg-final { scan-lto-assembler {\.section[ \t]+\.text\.same_fn\.lto_priv\.1[, \t"]} } } */
