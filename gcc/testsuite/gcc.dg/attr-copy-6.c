/* PR middle-end/88546 - Copy attribute unusable for weakrefs
   { dg-do compile }
   { dg-skip-if "Attributes not supported" { { hppa*-*-hpux* } && { ! lp64 } } }
   { dg-options "-O2 -Wall" }
   { dg-require-alias "" }
   { dg-require-weak "" } */

#define ATTR(...)   __attribute__ ((__VA_ARGS__))
#define ASRT(expr)   _Static_assert (expr, #expr)

/* Variable that is local to this translation unit but that can
   be modified from other units by calling reset_unit_local().  */
static int unit_local;

void reset_unit_local (void)
{
  unit_local = 0;
}

/* Attribute leaf implies that fleaf() doesn't modify unit_local().  */
ATTR (leaf, returns_nonnull)
void* fleaf_retnz (void);

/* Verify both attributes have been applied.  */
ASRT (__builtin_has_attribute (fleaf_retnz, leaf));
ASRT (__builtin_has_attribute (fleaf_retnz, returns_nonnull));

/* Verify that attribute leaf has the expected effect.  */
void call_fleaf_retnz (void)
{
  int i = unit_local;
  void *p = fleaf_retnz ();

  /* Expect both tests to be folded to false and the calls eliminated.  */
  extern void call_fleaf_retnz_test_leaf_eliminated (void);
  if (i != unit_local)
    call_fleaf_retnz_test_leaf_eliminated ();

  extern void call_fleaf_retnz_test_nonnull_eliminated (void);
  if (p == 0)
    call_fleaf_retnz_test_nonnull_eliminated ();
}


/* Verify that attribute copy copies the returns_nonnull attribute
   but doesn't try to copy attribute leaf which only applies to extern
   function.  */
static ATTR (copy (fleaf_retnz), weakref ("fleaf_retnz"))
void* fweakref_fleaf_retnz_copy (void);

ASRT (!__builtin_has_attribute (fweakref_fleaf_retnz_copy, leaf));
ASRT (__builtin_has_attribute (fweakref_fleaf_retnz_copy, returns_nonnull));

void call_fweakref_fleaf_retnz_copy (void)
{
  int i = unit_local;
  void *p = fweakref_fleaf_retnz_copy ();

  /* Since leaf is not copied, expect the following test not to be
     folded and the call to be emitted.  */
  extern void call_fweakref_test_leaf_emitted (void);
  if (i != unit_local)
    call_fweakref_test_leaf_emitted ();

  /* Expect the following test to be folded to false and the call
     eliminated.  */
  extern void call_fweakref_fleaf_nonnull_eliminated (void);
  if (p == 0)
    call_fweakref_fleaf_nonnull_eliminated ();
}

/* This is reduced from libgfortran/runtime/error.c.  Verify it
   doesn't trigger warnings and that the noreturn bit is copied
   to the alias by verifying that calling the alias in a non-void
   function with no return statement isn't diagnosed.  */

extern _Noreturn void fnoreturn (void);

extern __typeof (fnoreturn)
  ATTR (visibility ("hidden"))
  fnoreturn __asm__ ("fnoreturn_name");

void fnoreturn (void)
{
  __builtin_abort ();
}

extern __typeof (fnoreturn)
  ATTR (alias ("fnoreturn_name"), copy (fnoreturn))
  fnoreturn_alias;

int call_fnoreturn_alias (void)
{
  fnoreturn_alias ();
}
