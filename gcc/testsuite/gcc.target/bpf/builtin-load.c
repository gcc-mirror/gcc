/* { dg-do compile } */
/* { dg-options "-O0 -std=gnu99" } */

void foo ()
{
  long long ll, off;

  /* Indirect.  */
  ll = __builtin_bpf_load_byte (off);
  ll = __builtin_bpf_load_half (off);
  ll = __builtin_bpf_load_word (off);

  /* Absolute.  */
  ll = __builtin_bpf_load_byte (0);
  ll = __builtin_bpf_load_half (4);
  ll = __builtin_bpf_load_word (8);
}

/* { dg-final { scan-assembler "ldindb\t%r.,0.*ldindh\t%r.,0.*ldindw\t%r.,0" } } */
/* { dg-final { scan-assembler "ldabsb\t0.*ldabsh\t4.*ldabsw\t8" } } */
