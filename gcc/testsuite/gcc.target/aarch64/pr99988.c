/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mbranch-protection=standard" } */
/* { dg-final { scan-assembler-times {bti j} 13 } } */
int a;
int c();
int d();
int e();
int f();
int g();
void h() {
  switch (a) {
  case 0:
  case 56:
  case 57:
    break;
  case 58:
  case 59:
  case 61:
  case 62:
    c();
  case 64:
  case 63:
    d();
  case 66:
  case 65:
    d();
  case 68:
  case 67:
    d();
  case 69:
  case 70:
    d();
  case 71:
  case 72:
  case 88:
  case 87:
    d();
  case 90:
  case 89:
    d();
  case 92:
  case 1:
    d();
  case 93:
  case 73:
  case 4:
    e();
  case 76:
  case 5:
    f();
  case 7:
  case 8:
  case 84:
  case 85:
    break;
  case 6:
  case 299:
  case 9:
  case 80:
  case 2:
  case 3:
    e();
  default:
    g();
  }
}
