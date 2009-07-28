/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-mno-ms-bitfields" { target i?86-*-netware } } */

struct t
{
  char a:4;
  char b:8 __attribute__ ((packed));
  char c:4;
}; /* { dg-message "note: Offset of packed bit-field 'b' has changed in GCC 4.4" "" { target pcc_bitfield_type_matters } } */

int assrt[sizeof (struct t) == 2 ? 1 : -1];
