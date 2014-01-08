/* { dg-do compile { target { ! "m68k*-*-* mmix*-*-* mep*-*-* bfin*-*-* v850*-*-* picochip*-*-* moxie*-*-* cris*-*-* m32c*-*-* fr30*-*-* mcore*-*-* powerpc*-*-* xtensa*-*-* arc*-*-* hppa*-*-*"} } } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* Skip on ARM Cortex-M, where LOGICAL_OP_NON_SHORT_CIRCUIT is set to false,
   leading to two conditional jumps when evaluating an && condition.  Forwprop1
   is not able to optimize this.  */
/* { dg-skip-if "" { arm_cortex_m } } */

extern char *frob (void);
extern _Bool testit (void);

test (int code)
{
  char *temp = frob ();
  int rotate = (code == 22);
  if (temp == 0 && !rotate)
    oof ();
}

test_2 (int code)
{
  char *temp = frob ();
  int rotate = (code == 22);
  if (!rotate && temp == 0)
    oof ();
}


test_3 (int code)
{
  char *temp = frob ();
  int rotate = (code == 22);
  if (!rotate || temp == 0)
    oof ();
}


test_4 (int code)
{
  char *temp = frob ();
  int rotate = (code == 22);
  if (temp == 0 || !rotate)
    oof ();
}


test_5 (int code)
{
  _Bool temp = testit ();
  _Bool rotate = (code == 22);
  if (temp == 0 && !rotate)
    oof ();
}

test_6 (int code)
{
  _Bool temp = testit ();
  _Bool rotate = (code == 22);
  if (!rotate && temp == 0)
    oof ();
}


test_7 (int code)
{
  _Bool temp = testit ();
  _Bool rotate = (code == 22);
  if (!rotate || temp == 0)
    oof ();
}


test_8 (int code)
{
  _Bool temp = testit ();
  _Bool rotate = (code == 22);
  if (temp == 0 || !rotate)
    oof ();
}

/* { dg-final { scan-tree-dump-times "Replaced" 8 "forwprop1"} } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */

