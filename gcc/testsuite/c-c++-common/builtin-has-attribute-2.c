/* Verify __builtin_has_attribute return value for types.
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" }
   { dg-options "-Wall -Wno-narrowing -Wno-unused-local-typedefs -ftrack-macro-expansion=0" { target c++ } }  */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(__builtin_has_attribute (sym, attr) == expect)]

struct ATTR (packed) Packed { char c; int i; };

void fvoid (void);
struct Packed fpacked (void);

union OrdinaryUnion { void *p; int i; };
union ATTR (transparent_union) TransparentUnion { void *p; int i; };

/* Exercise __builtin_has_attribute with the first argument that
   is a type.  */

void test_type (int n)
{
  /* Verify both forms of the attribute spelling.  Unlike the attribute
     keyword that can be spelled three ways (with either leading or
     trailing underscores, or with both), attribute names can only be
     spelled two ways.  */
  A (0, int, aligned);
  A (0, int, __aligned__);

  A (0, int, aligned (1));
  A (0, int, aligned (2));
  A (0, int[1], aligned);
  A (0, int[1], aligned (2));
  A (0, int[n], aligned);
  A (0, int[n], aligned (4));

  /* Again, verify both forms of the attribute spelling.  */
  A (1, ATTR (aligned) char, aligned);
  A (1, ATTR (aligned (2)) short, aligned);
  A (1, ATTR (aligned (4)) int, __aligned__);

  A (0, int ATTR (aligned (4)), aligned (2));
  A (0, int ATTR (aligned (2)), aligned (4));
  /* GCC retains both attributes when the type is defined in the builtin.  */
  A (1, int ATTR (aligned (2), aligned (4)), aligned (2));
  A (1, int ATTR (aligned (2), aligned (4)), aligned (4));
  /* The following fails due to bug 87524.
     A (1, int ATTR (aligned (4), aligned (2))), aligned (4)); */
  A (0, int ATTR (aligned (4), aligned (2)), aligned (8));

  A (1, int ATTR (aligned (8)), aligned (1 + 7));

  enum { eight = 8 };
  A (1, int ATTR (aligned (8)), aligned (eight));
  A (1, int ATTR (aligned (eight)), aligned (1 + 7));

  struct NotPacked { char c; int i; };
  A (0, struct NotPacked, packed);
  A (1, struct Packed, packed);

  /* Exercise types returned from a function.  */
  A (0, fvoid (), packed);
  A (1, fpacked (), packed);

  struct ATTR (aligned (2), packed) Aligned2Packed { char c; int i; };
  A (1, struct Aligned2Packed, aligned);
  A (1, struct Aligned2Packed, aligned (2));
  A (0, struct Aligned2Packed, aligned (4));
  A (1, struct Aligned2Packed, packed);

  A (0, int, may_alias);
  A (1, ATTR (may_alias) int, may_alias);

  A (0, char, warn_if_not_aligned (1));
  A (0, char, warn_if_not_aligned (2));

  A (1, ATTR (warn_if_not_aligned (2)) char, warn_if_not_aligned);
  A (0, ATTR (warn_if_not_aligned (2)) char, warn_if_not_aligned (1));
  A (1, ATTR (warn_if_not_aligned (2)) char, warn_if_not_aligned (2));
  A (0, ATTR (warn_if_not_aligned (2)) char, warn_if_not_aligned (4));

  A (0, union OrdinaryUnion, transparent_union);

  A (1, union TransparentUnion, transparent_union);
  A (1, const union TransparentUnion, transparent_union);
}

/* Exercise __builtin_has_attribute with the first argument that
   is a typedef.  */

void test_typedef (int n)
{
  typedef char A1[1];
  A (0, A1, aligned);
  A (0, A1, aligned (1));
  A (0, A1, aligned (2));

  typedef char An[n];
  A (0, An, aligned);
  A (0, An, aligned (1));
  A (0, An, aligned (2));

  typedef ATTR (aligned (8)) short AI8;
  A (1, AI8, aligned);
  A (0, AI8, aligned (4));
  A (1, AI8, aligned (8));
  A (0, AI8, aligned (16));

  A (1, const AI8, aligned);
  A (1, const volatile AI8, aligned);

  typedef ATTR (aligned (2), aligned (8), aligned (16)) int AI16;
  A (1, AI16, aligned);
  A (0, AI16, aligned (1));
  A (0, AI16, aligned (2));
  A (0, AI16, aligned (4));
  A (0, AI16, aligned (8));
  A (1, AI16, aligned (16));
  A (0, AI16, aligned (32));

  typedef const AI16 CAI16;
  A (1, CAI16, aligned);
  A (0, CAI16, aligned (1));
  A (1, CAI16, aligned (16));

  typedef int I;
  A (0, I, may_alias);
  A (0, AI8, may_alias);

  typedef ATTR (may_alias) int MAI;
  A (1, MAI, may_alias);

  typedef ATTR (aligned (4), may_alias) char A4MAC;
  A (0, A4MAC, aligned (0));    /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, A4MAC, aligned (1));
  A (0, A4MAC, aligned (2));
  A (1, A4MAC, aligned (4));
  A (0, A4MAC, aligned (8));
  A (1, A4MAC, may_alias);

  typedef ATTR (may_alias, aligned (8)) char A8MAC;
  A (1, A8MAC, aligned);
  A (0, A8MAC, aligned (0));    /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, A8MAC, aligned (1));
  A (0, A8MAC, aligned (2));
  A (0, A8MAC, aligned (4));
  A (1, A8MAC, aligned (8));
  A (0, A8MAC, aligned (16));
  A (1, A8MAC, may_alias);

  typedef ATTR (may_alias) const AI8 CMAI8;
  A (1, CMAI8, aligned);
  A (1, CMAI8, may_alias);
  A (0, CMAI8, aligned (4));
  A (1, CMAI8, aligned (8));

  typedef void Fnull (void*, void*, void*);
  A (0, Fnull, nonnull);
  A (0, Fnull, nonnull (1));
  A (0, Fnull, nonnull (2));
  A (0, Fnull, nonnull (3));

  typedef ATTR (nonnull) Fnull Fnonnull;
  A (1, Fnonnull, nonnull);
  A (1, Fnonnull, nonnull (1));
  A (1, Fnonnull, nonnull (2));
  A (1, Fnonnull, nonnull (3));

  typedef ATTR (nonnull (2)) void Fnonnull_2 (void*, void*, void*);
  A (0, Fnonnull_2, nonnull);
  A (0, Fnonnull_2, nonnull (1));
  A (1, Fnonnull_2, nonnull (2));
  A (0, Fnonnull_2, nonnull (3));

  typedef ATTR (nonnull (1), nonnull (2), nonnull (3))
    void Fnonnull_1_2_3 (void*, void*, void*);

  /* The following fails because  the built-in doesn't recognize that
     a single nonnull with no arguments is the same as one nonnull for
     each function parameter.  Disable the testing for now.
     A (1, Fnonnull_1_2_3, nonnull);
  */
  A (1, Fnonnull_1_2_3, nonnull (1));
  A (1, Fnonnull_1_2_3, nonnull (2));
  A (1, Fnonnull_1_2_3, nonnull (3));

  typedef void Freturns (void);
  A (0, Fnull, noreturn);
  A (0, Freturns, noreturn);

  typedef ATTR (warn_if_not_aligned (8)) char CWA8;
  A (0, CWA8, warn_if_not_aligned (2));
  A (0, CWA8, warn_if_not_aligned (4));
  A (1, CWA8, warn_if_not_aligned (8));
  A (0, CWA8, warn_if_not_aligned (16));

  typedef union OrdinaryUnion OrdUnion;
  A (0, OrdUnion, transparent_union);

  /* The attribute is ignored on typedefs but GCC fails to diagnose
     it (see bug ).  */
  typedef union ATTR (transparent_union)
    OrdinaryUnion TransUnion;   /* { dg-warning "\\\[-Wattributes" "pr87578" { xfail { ! { c++ } } } } */
  A (0, TransUnion, transparent_union);
}
