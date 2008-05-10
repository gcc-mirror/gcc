/* { dg-do compile } */
/* Currently ICEs for MIPS; see PR33642.  */
/* { dg-skip-if "PR33642" { mips*-*-* } { "*" } { "" } } */
/* { dg-options "-frtl-abstract-sequences" } */

int a, b, e;
unsigned char *c;
void foo()
{
  int d = 13;
  b = -1;   
  switch (e) {
    case 1:
      b++; c[b] = (unsigned char)d;
      break;
    case 2:
      b++; c[b] = (unsigned char)d;
      b++; c[b] = (unsigned char)d;
      break;
    case 3:
      b++; c[b] = (unsigned char)d;
      b++; c[b] = (unsigned char)d;
      b++; c[b] = (unsigned char)d;
      break;
    default:
      a = 1;
      b++; c[b] = (unsigned char)d;
      b++; c[b] = (unsigned char)d;
      b++; c[b] = (unsigned char)d;
      b++; c[b] = (unsigned char)d;
  }
}
