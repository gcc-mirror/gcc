/* Check if the final SO STABS record goes into the .text section.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com> */

/* { dg-do compile { target stabs } } */
/* { dg-options "-gstabs" } */
/* { dg-additional-options "-Wno-objc-root-class" } */

@interface MyClass
+ newWithArg: arg;
@end

@implementation MyClass
+ newWithArg: arg
{
}
@end

/* See PR target/52152 for the xfail.  */
/* { dg-final { scan-assembler "(.SUBSPA.*\[\$\]CODE\[\$\]|.text\"?)\n\t.stabs.*100,0,0,(\[\.\$\])?L?L\[\$\]?etext\[0-9\]*\n(\[\.\$\])?L?L\[\$\]?etext" { xfail mips*-*-elf* } } } */
