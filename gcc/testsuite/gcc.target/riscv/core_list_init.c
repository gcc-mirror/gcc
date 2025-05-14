/* { dg-do compile } */
/* { dg-options "-fext-dce -fdump-rtl-ext_dce" } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */
/* { dg-skip-if "" { *-*-* } { "-O0"} } */

unsigned short
core_list_init (int size, short seed) {

  for (int i = 0; i < size; i++) {
    unsigned short datpat = ((unsigned short)(seed ^ i) & 0xf);
    unsigned short dat = (datpat << 3) | (i & 0x7);
    if (i > seed) {
      return dat;
    }
  }

  return 0;

}
