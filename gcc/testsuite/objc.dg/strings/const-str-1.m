/* Test errors for constant strings.  */
/* { dg-do compile } */
/* { dg-options "-mno-constant-cfstrings" { target *-*-darwin* } } */

#ifdef __cplusplus
extern void baz(...);
#endif

void foo()
{
  baz(@"hiya");  /* { dg-error "annot find interface declaration" } */
}

@interface NXConstantString
{
  void *isa;
  char *str;
  int len;
}
@end

void bar()
{
  baz(@"howdah");
}
