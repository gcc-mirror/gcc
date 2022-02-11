/* Verify that we can compile with debug info.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-g" } } */
extern int var1;
extern int var2;
extern void sub2 (void);

void __attribute__ ((interrupt))
sub (void)
{
  if (var1)
    var2 = 0;
  else
    sub2 ();
}
