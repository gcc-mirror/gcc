// Verify the correctness of folding relational expressions involving
// pointers to array elements and struct data members and null pointers.
// Although the C semantics of relational expressions are only defined
// for pointers to objects, C++ makes them well-defined when
// (nullptr < p) yields true.  See the discussion of the patch for
// c++/67376 on gcc-patches for additional background.

// { dg-do compile { target c++11 } }
// { dg-options "-fdelete-null-pointer-checks -fdump-tree-optimized" }

// Runtime assert.  Used for potentially invalid expressions.
#define RA(e)  ((e) ? (void)0 : __builtin_abort ())

// Static assert.  Used for valid core constant expressions.
#define SA(e)  static_assert ((e), #e)

void test_first_array_element ()
{
  static constexpr int a[] = { 0 };
  constexpr const int *null = 0;
  constexpr const int *pi = a;

  // The following are valid constant expressions since in &*pi
  // the '&*' "cancel each other out."
  SA (!(null == &*pi));
  SA ( (null != &*pi));

  // The validity of the relational expressions involving null
  // pointers in a constexpr context is questionable.  Use a run
  // time assertion to verify these.
  RA ( (null <  &*pi));
  RA ( (null <= &*pi));
  RA (!(null >  &*pi));
  RA (!(null >= &*pi));

  SA (!(&*pi == null));
  SA ( (&*pi != null));
  RA (!(&*pi <  null));
  RA (!(&*pi <= null));
  RA ( (&*pi >  null));
  RA ( (&*pi >= null));

  // The following are valid constant expressions since &pi [0] is
  // equivalent to &*pi.
  SA (!(null == &pi [0]));
  SA ( (null != &pi [0]));
  RA ( (null <  &pi [0]));
  RA ( (null <= &pi [0]));
  RA (!(null >  &pi [0]));
  RA (!(null >= &pi [0]));

  SA (!(&pi [0] == null));
  SA ( (&pi [0] != null));
  RA (!(&pi [0] <  null));
  RA (!(&pi [0] <= null));
  RA ( (&pi [0] >  null));
  RA ( (&pi [0] >= null));
}

void test_first_null_array_element ()
{
  constexpr const int *pi = 0;
  constexpr const int *qi = 0;

  // The following are valid constant expressions since in &*qi
  // the '&*' "cancel each other out."
  SA ( (pi == &*qi));
  SA (!(pi != &*qi));

  // The validity of the relational expressions involving null
  // pointers in a constexpr context is questionable.
  RA (!(pi <  &*qi));
  RA ( (pi <= &*qi));
  RA (!(pi >  &*qi));
  RA ( (pi >= &*qi));

  SA ( (&*qi == pi));
  SA (!(&*qi != pi));
  RA (!(&*qi <  pi));
  RA ( (&*qi <= pi));
  RA (!(&*qi >  pi));
  RA ( (&*qi >= pi));

  // The following are valid constant expressions since &qi [0] is
  // equivalent to &*qi.
  SA ( (pi == &qi [0]));
  SA (!(pi != &qi [0]));
  RA (!(pi <  &qi [0]));
  RA ( (pi <= &qi [0]));
  RA (!(pi >  &qi [0]));
  RA ( (pi >= &qi [0]));

  SA ( (&qi [0] == pi));
  SA (!(&qi [0] != pi));
  RA (!(&qi [0] <  pi));
  RA ( (&qi [0] <= pi));
  RA (!(&qi [0] >  pi));
  RA ( (&qi [0] >= pi));
}

void test_first_struct_member ()
{
  static struct S { int a, b; } s = { 0, 0 };

  constexpr const int *p = 0;
  constexpr const S   *q = &s;

  SA (!(p == &q->b));
  SA ( (p != &q->b));
  RA ( (p <  &q->b));
  RA ( (p <= &q->b));
  RA (!(p >  &q->b));
  RA (!(p >= &q->b));

  SA (!(&q->b == p));
  SA ( (&q->b != p));
  RA (!(&q->b <  p));
  RA (!(&q->b <= p));
  RA ( (&q->b >  p));
  RA ( (&q->b >= p));
}

// Expect all runtime asserts to have been eliminated as a result
// of the tested expressions constant folded into true.
// { dg-final { scan-assembler-not "abort" } }
