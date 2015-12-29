/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic" } */

__thread unsigned char tls_array[64];

unsigned char
tls_array_lookup_with_negative_constant(long long int position) {
  return tls_array[position - 1];
}

/* { dg-final { scan-assembler "mov(b|zbl)\[ \t\](%fs:)?(-1\\+)?tls_array@tpoff(-1)?\\(%" } } */
