/* Origin: <hp@bitrange.com>
   Test that MMIX -mtoplevel-symbols cause a ":" to be prepended on the
   right symbols and only there.  */
/* { dg-do compile { target mmix-*-* } } */
/* { dg-options "-mtoplevel-symbols" } */

static int static_variable = 1;
int global_initialized_variable = 2;
extern int extern_declared_variable;
int common_variable;
extern int extern_declared_function (void);
static int static_function (void);

int global_defined_function ()
{
  static int static_variable_in_function = 2009;
  return
    static_variable
    + static_variable_in_function++
    + global_initialized_variable
    + common_variable
    + extern_declared_function ()
    + static_function ();
}

static int
static_function (void)
{
  if (extern_declared_variable)
    return 42;
  else
    return 42 + global_defined_function ();
}

/* { dg-final { scan-assembler-not ":static_variable" } } */
/* { dg-final { scan-assembler-not "(^|\[^:\])global_initialized_variable" } } */
/* { dg-final { scan-assembler-not "\[^:\]extern_declared_variable" } } */
/* { dg-final { scan-assembler-not "\[^:\]common_variable" } } */
/* { dg-final { scan-assembler-not "\[^:\]extern_declared_function" } } */
/* { dg-final { scan-assembler-not ":static_function" } } */
/* { dg-final { scan-assembler-not "(^|\[^:\])global_defined_function" } } */
/* { dg-final { scan-assembler-not "\[^:\]extern_declared_variable" } } */
