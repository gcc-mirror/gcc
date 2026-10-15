/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Oz" "-Os" } } */
/* { dg-options "-mtune=arc-v-rpx-100-series -march=rv64imd -mabi=lp64d -fdump-rtl-sched2 -Wno-int-to-pointer-cast" } */

void
foo (int *a)
{
  a[2] = a[1] * a[0];

}

void foo_high(long *b) {
  b[2] = ((__int128)b[1] * (__int128)b[0]) >> 64;
}

void foo_low(long *b) {
  b[2] = b[1] * b[0];
}

void
bar (int *a)
{
  int *addr = (int *) (a[1] * a[2]);
  *addr = 0;
}

void
bar_low (long *a)
{
  long *addr = (long *) (a[1] * a[2]);
  *addr = 0;
}

void
bar_high (long *a)
{
  long *addr = (long *) (((__int128) a[1] * (__int128) a[2]) >> 64);
  *addr = 0;
}

int
baz (int *a)
{
  int *addr = (int *) (a[1] * a[2]);
  return *addr;
}

long
baz_low (long *a)
{
  long *addr = (long *) (a[1] * a[2]);
  return *addr;
}

long baz_high(long *a) {
  long *addr = (long *)(((__int128) a[1] * (__int128) a[2]) >> 64);
  return *addr;
}

/* Verify MULW to STORE data delay is 1 cycle. */
/* { dg-final { scan-rtl-dump "(?s);; Function foo.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+2-->.*\\\[.*0x8\\\]=" "sched2" } } */
/* Verify MULW to STORE address delay is 7 cycles. */
/* { dg-final { scan-rtl-dump "(?s);; Function bar.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+8-->.*\\\[.*\\\]=0" "sched2" } } */
/* Verify MULW to LOAD address delay is 7 cycles. */
/* { dg-final { scan-rtl-dump "(?s);; Function baz.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+8-->.*=.*\\\[.*\\\]" "sched2" } } */

/* Verify MUL to STORE data delay is 1 cycle. */
/* { dg-final { scan-rtl-dump "(?s);; Function foo_low.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+2-->.*\\\[.*0x10\\\]=" "sched2" } } */
/* Verify MUL to STORE address delay is 9 cycles. */
/* { dg-final { scan-rtl-dump "(?s);; Function bar_low.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+10-->.*\\\[.*\\\]=0" "sched2" } } */
/* Verify MUL to LOAD address delay is 9 cycles. */
/* { dg-final { scan-rtl-dump "(?s);; Function baz_low.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+10-->.*=.*\\\[.*\\\]" "sched2" } } */

/* Verify MULH to STORE data delay is 1 cycle. */
/* { dg-final { scan-rtl-dump "(?s);; Function foo_high.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+2-->.*\\\[.*0x10\\\]=" "sched2" } } */
/* Verify MULH to STORE address delay is 10 cycles. */
/* { dg-final { scan-rtl-dump "(?s);; Function bar_high.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+11-->.*\\\[.*\\\]=0" "sched2" } } */
/* Verify MULH to LOAD address delay is 10 cycles. */
/* { dg-final { scan-rtl-dump "(?s);; Function baz_high.*?;;\\s+1-->.*i\\s+\\d+\\s+.*\\*.*:\\(arcv.*?;;\\s+11-->.*=.*\\\[.*\\\]" "sched2" } } */
