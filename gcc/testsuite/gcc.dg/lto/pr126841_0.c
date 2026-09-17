/* { dg-lto-do link } */
/* { dg-require-effective-target elf } */
/* { dg-require-effective-target named_sections } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target shared } */
/* { dg-require-linker-plugin "" } */
/* { dg-lto-options { { -O2 -flto -fPIC -shared -ffunction-sections -save-temps } } } */

static int
same_fn (int x)
{
  return x * 33 + 7;
}

int (*a_callback) (int) = same_fn;

/* The exported function keeps the unsuffixed section, and each privatized
   function must have its own section.  */
/* { dg-final { scan-lto-assembler {\.section[ \t]+\.text\.same_fn[, \t"]} } } */
/* { dg-final { scan-lto-assembler {\.section[ \t]+\.text\.same_fn\.lto_priv\.0[, \t"]} } } */
/* { dg-final { scan-lto-assembler {\.section[ \t]+\.text\.same_fn\.lto_priv\.1[, \t"]} } } */
/* { dg-final { scan-lto-assembler {\.section[ \t]+\.text\.same_fn\.lto_priv\.2[, \t"]} } } */
