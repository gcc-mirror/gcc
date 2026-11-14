/* { dg-do run } */
/* { dg-options "-O2 -fno-early-inlining -fno-partial-inlining -fdump-ipa-fnsummary-details -fdump-ipa-inline-details" } */

__attribute__((noipa)) static void
sink (int x)
{
  (void) x;
}

int
test_sw_loop (const char *p)
{
  int acc = 0;
  while (*p)
    {
      switch (*p)
	{
	case '%':
	  sink (1);
	  sink (11);
	  sink (111);
	  acc += 1;
	  break;
	case '[':
	  sink (2);
	  sink (12);
	  acc += 2;
	  break;
	case '!':
	  sink (3);
	  sink (13);
	  acc += 3;
	  break;
	case '?':
	  sink (4);
	  sink (14);
	  acc += 4;
	  break;
	default:
	  break;
	}
      p++;
    }
  return acc;
}

int
test_sw_range (const char *p, int n)
{
  int acc = 0;
  for (int i = 0; i < n; i++)
    {
      switch (p[i])
	{
	case 0 ... 'f':
	  acc += 1;
	  break;
	case 'z':
	  sink (21);
	  sink (22);
	  sink (23);
	  sink (24);
	  sink (25);
	  sink (26);
	  sink (31);
	  acc += 2;
	  break;
	default:
	  sink (27);
	  sink (28);
	  sink (29);
	  sink (30);
	  sink (32);
	  sink (33);
	  sink (34);
	  acc += 100;
	  break;
	}
    }
  return acc;
}

int
test_sw_phi (const char *p, int i)
{
  int r;
  switch (p[i])
    {
    case '%':
      sink (31);
      sink (32);
      sink (33);
      sink (34);
      r = 10;
      break;
    case '[':
      sink (35);
      sink (36);
      sink (37);
      sink (38);
      r = 20;
      break;
    case '!':
      sink (39);
      sink (40);
      sink (41);
      sink (42);
      r = 30;
      break;
    default:
      r = 0;
      break;
    }
  int t = (r + 1) * (r + 3);
  t ^= (t + 5) * (r + 7);
  t += (r != 0 ? 17 : 0);
  if (r != 0)
    sink (t);
  return t;
}

__attribute__((noinline)) int
caller_loop_const (void)
{
  return test_sw_loop ("plain_identifier");
}

__attribute__((noinline)) int
caller_loop_nonconst (void)
{
  return test_sw_loop ("plain %s");
}

__attribute__((noinline)) int
caller_range_const (void)
{
  return test_sw_range ("deadbeef", 8);
}

__attribute__((noinline)) int
caller_range_nonconst (void)
{
  return test_sw_range ("az~!", 4);
}

__attribute__((noinline)) int
caller_phi_const (int i)
{
  return test_sw_phi ("hello", i);
}

__attribute__((noinline)) int
caller_phi_nonconst (int i)
{
  return test_sw_phi ("hi%!", i);
}

int
main (void)
{
  if (caller_loop_const () != 0)
    __builtin_abort ();
  if (caller_loop_nonconst () != 1)
    __builtin_abort ();
  if (caller_range_const () != 8)
    __builtin_abort ();
  if (caller_range_nonconst () != 104)
    __builtin_abort ();
  if (caller_phi_const (1) != 59)
    __builtin_abort ();
  if (caller_phi_nonconst (2) == 59)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op0\\\[ref maybe_modified unknown_offset\\\] == 33 \\|\\| op0\\\[ref maybe_modified unknown_offset\\\] == 37 \\|\\| op0\\\[ref maybe_modified unknown_offset\\\] == 63 \\|\\| op0\\\[ref maybe_modified unknown_offset\\\] == 91" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op0\\\[ref maybe_modified unknown_offset\\\] > 102\[^\n\]*op0\\\[ref maybe_modified unknown_offset\\\] == 122" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "phi predicate: \[^\n\]*op0\\\[ref unknown_offset\\\] == 33 \\|\\| op0\\\[ref unknown_offset\\\] == 37 \\|\\| op0\\\[ref unknown_offset\\\] == 91" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "Inlined test_sw_loop\[^\n\]* into caller_loop_const/" "inline" } } */
/* { dg-final { scan-ipa-dump "Inlined test_sw_range\[^\n\]* into caller_range_const/" "inline" } } */
/* { dg-final { scan-ipa-dump "Inlined test_sw_phi\[^\n\]* into caller_phi_const/" "inline" } } */
/* { dg-final { scan-ipa-dump-not "Inlined test_sw_loop\[^\n\]* into caller_loop_nonconst/" "inline" } } */
/* { dg-final { scan-ipa-dump-not "Inlined test_sw_range\[^\n\]* into caller_range_nonconst/" "inline" } } */
/* { dg-final { scan-ipa-dump-not "Inlined test_sw_phi\[^\n\]* into caller_phi_nonconst/" "inline" } } */
