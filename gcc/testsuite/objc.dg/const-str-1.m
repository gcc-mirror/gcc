/* Test errors for constant strings.  */
/* { dg-do compile } */
/* { dg-options "-fgnu-runtime" } */

void foo()
{
  baz(@"hiya");  /* { dg-error "annot find interface declaration" } */
}

@interface NXConstantString
@end

void bar()
{
  baz(@"howdah");
}
