/* Check if the final SO STABS record goes into the .text section.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com> */

/* { dg-do compile } */
/* { dg-skip-if "No stabs" { mmix-*-* *-*-aix* alpha*-*-* hppa*64*-*-* ia64-*-* } { "*" } { "" } } */
/* { dg-options "-gstabs" } */

@interface MyClass
+ newWithArg: arg;
@end

@implementation MyClass
+ newWithArg: arg
{
}
@end

/* See PR 52152 for the xfail.  */
/* { dg-final { scan-assembler "(.SUBSPA.*\[\$\]CODE\[\$\]|.text\"?)\n\t.stabs.*100,0,0,(\[\.\$\])?L?L\[\$\]?etext\[0-9\]*\n(\[\.\$\])?L?L\[\$\]?etext" { xfail mips*-*-elf* } } } */
