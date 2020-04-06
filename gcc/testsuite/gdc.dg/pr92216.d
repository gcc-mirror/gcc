// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=92216
// { dg-options "-I $srcdir/gdc.dg" }
// { dg-do compile }
// { dg-final { scan-assembler "_DT(4|8|16)_D7imports7pr922161B8__mixin24getSMFZPv\[: \t\n\]" } }
// { dg-final { scan-assembler-not "(.globl|.global)\[ 	\]+_DT(4|8|16)_D7imports7pr922161B8__mixin24getSMFZPv" } }
module pr92216;

private import imports.pr92216;

class C : B
{
    protected override void getStruct() {}
}
