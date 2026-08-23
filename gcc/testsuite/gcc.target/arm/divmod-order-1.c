/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-mthumb" } { "" } } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" } { "-mcpu=arm1176jzf-s" } } */
/* { dg-skip-if "incompatible options" { arm*-*-* } { "-march=*" } { "" } } */
/* { dg-options "-Os -marm -mcpu=arm1176jzf-s" } */

/* expand_divmod remembers the last division it expanded so that a following
   modulo by the same constant can reuse the quotient.  That state used to be a
   file-static that was never reset, so it carried from one function into the
   next: m1 and m2 below are character for character identical, yet m1 saw the
   state left behind by d and m2 saw the state left behind by m1, and the two
   compiled differently.  Reordering the functions in the source changed the
   generated code.

   Both must now be expanded the same way.  */

int d  (int x) { return x / 3; }
int m1 (int x) { return x % 3; }
int m2 (int x) { return x % 3; }

/* m1 and m2 are identical, so they must make the same number of calls.  d is a
   plain division and is expanded inline, so every libcall here comes from the
   two modulo functions: one each, never one and none.  */
/* { dg-final { scan-assembler-times "bl\\s+__aeabi_idivmod" 2 } } */
