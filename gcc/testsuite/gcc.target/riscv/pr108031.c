/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fdump-rtl-expand -fdump-rtl-cse1" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32d -fdump-rtl-expand -fdump-rtl-cse1" { target { rv32 } } } */

struct s {
  int a;
  int b;
};

struct s s;

int f(void)
{
  return __atomic_fetch_add(&s.a, 1, 0) + __atomic_fetch_add(&s.b, 1, 0);
}


struct s2 {
  long a;
  long b;
};

struct s2 s2;

long f2(void)
{
  return __atomic_fetch_add(&s2.a, 1, 0) + __atomic_fetch_add(&s2.b, 1, 0);
}

/* { dg-final { scan-rtl-dump-not "mem\[^\r\n]*lo_sum" "expand" } } */
/* { dg-final { scan-rtl-dump-not "mem\[^\r\n]*lo_sum" "cse1" } } */
