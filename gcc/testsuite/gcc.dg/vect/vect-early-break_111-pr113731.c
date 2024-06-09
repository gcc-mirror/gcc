/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_long } */
/* { dg-additional-options "-msse4.2" { target i?86-*-* x86_64-*-* } } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

char* inet_net_pton_ipv4_bits;
char inet_net_pton_ipv4_odst;
void __errno_location();
void inet_net_pton_ipv4();
void inet_net_pton() { inet_net_pton_ipv4(); }
void inet_net_pton_ipv4(char *dst, int size) {
  while ((inet_net_pton_ipv4_bits > dst) & inet_net_pton_ipv4_odst) {
    if (size-- <= 0)
      goto emsgsize;
    *dst++ = '\0';
  }
emsgsize:
  __errno_location();
}
