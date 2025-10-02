/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32" { target { rv32 } } } */

void test0(unsigned long* lock) {
  while (!__atomic_exchange_n(lock, 0, __ATOMIC_ACQUIRE));
}


void test1(unsigned* lock) {
  while (!__atomic_exchange_n(lock, 0, __ATOMIC_ACQUIRE));
}

/* { dg-final { scan-assembler-not "\tli" } } */
/* { dg-final { scan-assembler-times "\tamoswap...aq\t\[axt\]\[0-9\],zero," 2 } } */
/* { dg-final { scan-assembler-not "\tsext" } } */

