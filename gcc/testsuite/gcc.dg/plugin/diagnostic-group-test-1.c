/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret" } */

extern void __emit_warning (const char *message);

static void test_1 (void)
{
  __emit_warning ("warning 1");
}

/* { dg-begin-multiline-output "" }
================================= BEGIN GROUP ==============================
PREFIX
   __emit_warning ("warning 1");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
PREFIX: message for note
PREFIX:  some more detail
PREFIX:   yet more detail
---------------------------------- END GROUP -------------------------------
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
================================= BEGIN GROUP ==============================
PREFIX: an unrelated message
---------------------------------- END GROUP -------------------------------
   { dg-end-multiline-output "" } */
