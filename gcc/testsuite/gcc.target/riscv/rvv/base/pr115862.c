/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcv_zvl512b -mabi=lp64d" } */

struct mallinfo2
{
  int arena;
  int ordblks;
  int smblks;
  int hblks;
  int hblkhd;
  int usmblks;
  int fsmblks;
  int uordblks;
  int fordblks;
  int keepcost;
};

struct mallinfo
{
  int arena;
  int ordblks;
  int smblks;
  int hblks;
  int hblkhd;
  int usmblks;
  int fsmblks;
  int uordblks;
  int fordblks;
  int keepcost;
};

struct mallinfo
__libc_mallinfo (void)
{
  struct mallinfo m;
  struct mallinfo2 m2;

  m.arena = m2.arena;
  m.ordblks = m2.ordblks;
  m.smblks = m2.smblks;
  m.hblks = m2.hblks;
  m.hblkhd = m2.hblkhd;
  m.usmblks = m2.usmblks;
  m.fsmblks = m2.fsmblks;
  m.uordblks = m2.uordblks;
  m.fordblks = m2.fordblks;
  m.keepcost = m2.keepcost;

  return m;
}

/* { dg-final { scan-assembler {vle32\.v} } } */
