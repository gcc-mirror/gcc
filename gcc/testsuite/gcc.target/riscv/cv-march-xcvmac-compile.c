/* { dg-do compile } */
/* { dg-require-effective-target cv_mac } */
/* { dg-options "-march=rv32i_xcvmac -mabi=ilp32" } */

extern int d;
extern int e;
extern int f;

int foo(int a, int b, int c)
{
  return __builtin_riscv_cv_mac_mac (a, b, c);
}

void foo0(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_machhsN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_machhsN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_machhsN (a, b, c, 31);
}

void foo1(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_machhsRN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_machhsRN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_machhsRN (a, b, c, 31);
}

void foo2(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_machhuN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_machhuN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_machhuN (a, b, c, 31);
}

void foo3(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_machhuRN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_machhuRN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_machhuRN (a, b, c, 31);
}

void foo4(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_macsN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_macsN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_macsN (a, b, c, 31);
}

void foo5(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_macsRN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_macsRN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_macsRN (a, b, c, 31);
}

void foo6(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_macuN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_macuN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_macuN (a, b, c, 31);
}

void foo7(int a, int b, int c)
{
  d = __builtin_riscv_cv_mac_macuRN (a, b, c, 0);
  e = __builtin_riscv_cv_mac_macuRN (a, b, c, 7);
  f = __builtin_riscv_cv_mac_macuRN (a, b, c, 31);
}

int foo8(int a, int b, int c)
{
  return __builtin_riscv_cv_mac_msu (a, b, c);
}

void foo9(int a, int b)
{
  d = __builtin_riscv_cv_mac_mulhhsN (a, b, 0);
  e = __builtin_riscv_cv_mac_mulhhsN (a, b, 7);
  f = __builtin_riscv_cv_mac_mulhhsN (a, b, 31);
}

void foo10(int a, int b)
{
  d = __builtin_riscv_cv_mac_mulhhsRN (a, b, 0);
  e = __builtin_riscv_cv_mac_mulhhsRN (a, b, 7);
  f = __builtin_riscv_cv_mac_mulhhsRN (a, b, 31);
}

void foo11(int a, int b)
{
  d = __builtin_riscv_cv_mac_mulhhuN (a, b, 0);
  e = __builtin_riscv_cv_mac_mulhhuN (a, b, 7);
  f = __builtin_riscv_cv_mac_mulhhuN (a, b, 31);
}

void foo12(int a, int b)
{
  d = __builtin_riscv_cv_mac_mulhhuRN (a, b, 0);
  e = __builtin_riscv_cv_mac_mulhhuRN (a, b, 7);
  f = __builtin_riscv_cv_mac_mulhhuRN (a, b, 31);
}

void foo13(int a, int b)
{
  d = __builtin_riscv_cv_mac_mulsN (a, b, 0);
  e = __builtin_riscv_cv_mac_mulsN (a, b, 7);
  f = __builtin_riscv_cv_mac_mulsN (a, b, 31);
}

void foo14(int a, int b)
{
  d = __builtin_riscv_cv_mac_mulsN (a, b, 0);
  e = __builtin_riscv_cv_mac_mulsN (a, b, 7);
  f = __builtin_riscv_cv_mac_mulsN (a, b, 31);
}

void foo15(int a, int b)
{
  d = __builtin_riscv_cv_mac_muluN (a, b, 0);
  e = __builtin_riscv_cv_mac_muluN (a, b, 7);
  f = __builtin_riscv_cv_mac_muluN (a, b, 31);
}

void foo16(int a, int b)
{
  d = __builtin_riscv_cv_mac_muluRN (a, b, 0);
  e = __builtin_riscv_cv_mac_muluRN (a, b, 7);
  f = __builtin_riscv_cv_mac_muluRN (a, b, 31);
}

/* { dg-final { scan-assembler-times "cv\.mac\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\)" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhsN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhsN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhsN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhsRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhsRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhsRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhuN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhuN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhuN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhuRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhuRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.machhuRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macsN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macsN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macsN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macsRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macsRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macsRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macuN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macuN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macuN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macuRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macuRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.macuRN\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.msu\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\)" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhsRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhsRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhsRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhuN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhuN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhuN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhuRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhuRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulhhuRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.mulsN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.muluN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.muluN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.muluN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "cv\.muluRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.muluRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.muluRN\t\[a-z\]\[0-9\],\[a-z\]\[0-9\],\[a-z\]\[0-9\],31" 1 } } */
