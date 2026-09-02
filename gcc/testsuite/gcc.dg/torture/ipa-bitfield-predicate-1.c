/* { dg-do run } */
/* { dg-require-effective-target le } */
/* { dg-require-effective-target int32plus } */

/* An IPA predicate condition on a bit-field was matched against the
   aggregate constant recorded for the byte containing it.  The two were
   considered interchangeable because TYPE_SIZE of a 3-bit bit-field type
   and of char are both 8 bits, so the byte was reinterpreted as the field
   with a VIEW_CONVERT_EXPR.  That reinterpretation keeps the least
   significant bits of the byte, which are the bit-field only under
   little-endian bit numbering.  Here the storage order is reversed, f7 is
   the *top* three bits of the byte, and the byte 0x24 is misread as -4
   instead of 1 -- turning the guard below into a false predicate and the
   call to shifter() into __builtin_unreachable.  */

struct __attribute__((scalar_storage_order("big-endian"))) S0
{
  signed f7 : 3;
  unsigned f6 : 5;
};

struct S1 { int a; struct S0 f3; char pad; short s; };

int g;

__attribute__((noipa)) void ext (int x) { g += x; }

__attribute__((noinline, noclone)) static long long
shifter (long long l, int r)
{
  if (l < 0 || r < 0 || r >= 32 || l > (0x7fffffffffffffffLL >> r))
    return l;
  return l << r;
}

static void
callee (struct S1 p, int n)
{
  ext (n);
  if ((int) p.f3.f7 >= 0)
    ext ((int) shifter (0x350631DD6B880108LL, (int) p.f3.f7));
  ext (n);
}

/* A second caller, so that callee is not inlined before IPA.  */
void other (struct S1 q, int n) { callee (q, n); }

int
main ()
{
  struct S1 l;
  l.a = 5;
  l.pad = 7;
  l.s = 9;
  *(char *) &l.f3 = 0x24;	/* reversed bit order: f7 = 1, f6 = 4 */
  if ((int) l.f3.f7 != 1)
    return 0;			/* not the layout this test is about */
  callee (l, 3);
  if (g != 3 + 3 + (int) (0x350631DD6B880108LL << 1))
    __builtin_abort ();
  return 0;
}
