// { dg-do compile }
// { dg-additional-options "-m64" }

// ptr to member data is like sizeof.
// ptr to member fn is like a struct.

struct A{};

typedef int A::*pmd;
typedef void (A::*pmf) ();

// { dg-final { scan-assembler ".extern .func \\(.param.u64 %value_out\\) _Z8dcl_rpmdv;" } }
pmd dcl_rpmd ();

// { dg-final { scan-assembler ".extern .func _Z8dcl_rpmfv \\(.param.u64 %in_ar0\\);" } }
pmf dcl_rpmf ();

// { dg-final { scan-assembler ".extern .func _Z8dcl_apmdM1Ai \\(.param.u64 %in_ar0\\);" } }
void dcl_apmd (pmd);

// { dg-final { scan-assembler ".extern .func _Z8dcl_apmfM1AFvvE \\(.param.u64 %in_ar0\\);" } }
void dcl_apmf (pmf);

void test_1 ()
{
  dcl_rpmd ();
  dcl_rpmf ();
  dcl_apmd (0);
  dcl_apmf (0);
}

// { dg-final { scan-assembler-times ".visible .func \\(.param.u64 %value_out\\) _Z8dfn_rpmdv(?:;|\[\r\n\]+\{)" 2 } }
pmd dfn_rpmd ()
{
  return 0;
}

// { dg-final { scan-assembler-times ".visible .func _Z8dfn_rpmfv \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
pmf dfn_rpmf ()
{
  return 0;
}

// { dg-final { scan-assembler-times ".visible .func _Z8dfn_apmdM1Ai \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
void dfn_apmd (pmd)
{
}

// { dg-final { scan-assembler-times ".visible .func _Z8dfn_apmfM1AFvvE \\(.param.u64 %in_ar0\\)(?:;|\[\r\n\]+\{)" 2 } }
void dfn_apmf (pmf)
{
}
