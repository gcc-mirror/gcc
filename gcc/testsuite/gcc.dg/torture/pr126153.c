/* { dg-do run { target le } } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2" } */

/* Conditions of IPA predicates record the position of an aggregate load as
   a bit offset, while the aggregate values of jump functions are indexed by
   byte offsets.  Truncating the former used to match the constant recorded
   for the byte containing the bit-field, which was then reinterpreted with
   a VIEW_CONVERT_EXPR as if it were the whole field.  Here that turned the
   value 1 of f7 into -4, the guard below into a false predicate and the
   call to shifter() into __builtin_unreachable.  */

struct S0 { unsigned f5 : 2; signed f7 : 3; unsigned f6 : 3; };
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
main (void)
{
  struct S1 l;
  l.a = 5;
  l.pad = 7;
  l.s = 9;
  *(char *) &l.f3 = 4;		/* f5 = 0, f7 = 1, f6 = 0 */
  callee (l, 3);
  if (g != 3 + 3 + (int) (0x350631DD6B880108LL << 1))
    __builtin_abort ();
  return 0;
}
