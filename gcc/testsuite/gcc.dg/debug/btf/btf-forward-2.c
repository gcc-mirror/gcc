/* PR111735.  Test BTF generation for forward-declared enum.

   The BTF representation for forward-declared enums is not formally
   defined, but the de-facto representation used by various tools is
   to encode them as a BTF_KIND_ENUM type with vlen=0 and size=0.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

enum Foo;
enum Foo *pfoo;

/* Check that there is one BTF_KIND_ENUM with vlen=0, and no BTF_KIND_FWD.  */
/* { dg-final { scan-assembler-times "\[\t \]0x6000000\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x7000000\[\t \]+\[^\n\]*btt_info" 0 } } */

/* Verify the reference to the enum-forward.  */
/* { dg-final { scan-assembler-times "btt_type: \\(BTF_KIND_ENUM 'Foo'\\)" 1 } } */
