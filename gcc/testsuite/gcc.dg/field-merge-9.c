/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that conversions and selections of similar bit ranges across different
   types don't prevent combination.  */

struct s1 {
  unsigned char b[2];
  unsigned char a;
} __attribute__ ((aligned (4)));

struct s2 {
  unsigned short b;
  unsigned char a;
} __attribute__ ((aligned (4)));

static const char le = __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ ? 1 : 0;

struct s1 p = { { -!le , -le }, 42 };
struct s2 q = { -2 << (__CHAR_BIT__ - 1), 42 };

void f (void) {
  if (0
      || p.a != q.a
      || p.b[!le] != (unsigned char)q.b
      || p.b[le] != (unsigned char)((q.b >> (__CHAR_BIT__ - 1)) >> 1)
      )
    __builtin_abort ();
}

int main () {
  if (sizeof (short) != 2)
    return 0;
  f ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing two comparisons" 2 "ifcombine" { target { ! { avr-*-* pru-*-* } } } } } */
