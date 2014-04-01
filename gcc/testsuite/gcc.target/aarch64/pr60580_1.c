/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer -fno-inline --save-temps" } */

void
func_leaf (void)
{
  int a = 0;
}

void
func_no_leaf (void)
{
  int a = 0;
  func_leaf ();
}

void
func1 (void)
{
  int a = 0;
  func_no_leaf ();
}

/*
 * This function calls XXX(), which modifies SP. This is incompatible to
 * -fomit-frame-pointer generated code as SP is used to access the frame.
 */
__attribute__ ((optimize("no-omit-frame-pointer")))
void
func2 (void)
{
  int a = 0;
  func_no_leaf ();
}

void
func3 (void)
{
  int a = 0;
  func_no_leaf ();
}

/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 1 } } */

/* { dg-final { cleanup-saved-temps } } */
