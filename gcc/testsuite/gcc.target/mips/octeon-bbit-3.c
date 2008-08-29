/* { dg-do compile } */
/* { dg-mips-options "-O2 -march=octeon" } */
/* { dg-final { scan-assembler-times "\tbbit\[01\]\t|\tbgez\t" 2 } } */
/* { dg-final { scan-assembler-not "ext\t" } } */

void abort (void);
void exit (int);

typedef unsigned long long ulong64;

typedef struct bitfield_s {
  ulong64 a:1;
  ulong64 b:29;
  ulong64 c:1;
  ulong64 d:15;
  ulong64 f:18;
} bitfield_t;

bitfield_t bar;

NOMIPS16 void
f ()
{
  foo(&bar);
  if (bar.a != 0x1)
    abort ();
  else if (!bar.c)
    abort ();
  else
    exit (0);
}
