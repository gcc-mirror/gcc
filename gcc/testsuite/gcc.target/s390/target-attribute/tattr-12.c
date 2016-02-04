/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-O3 -march=z13 -mstack-guard=0 -mstack-size=512" } */

extern char *bar(int x);

void da(int x)
{
  bar(x);
  bar(x + 1);
  /* { dg-final { scan-assembler-times "\ttmll\t%r15,256" 2 { target { lp64 } } } } */
  /* { dg-final { scan-assembler-times "\ttml\t%r15,384" 2 { target { ! lp64 } } } } */
}

#pragma GCC target("stack-size=1024,stack-guard=0")
void p1(int x)
{
  bar(x);
  bar(x + 1);
  /* { dg-final { scan-assembler-times "\ttmll\t%r15,768" 1 { target { lp64 } } } } */
  /* { dg-final { scan-assembler-times "\ttml\t%r15,896" 1 { target { ! lp64 } } } } */
}
#pragma GCC reset_options

#pragma GCC target("stack-size=2048,stack-guard=0")
void p0(int x)
{
  bar(x);
  bar(x + 1);
  /* { dg-final { scan-assembler-times "\ttmll\t%r15,1792" 1 { target { lp64 } } } } */
  /* { dg-final { scan-assembler-times "\ttml\t%r15,1920" 1 { target { ! lp64 } } } } */
}
#pragma GCC reset_options

__attribute__ ((target("stack-size=4096,stack-guard=0")))
void a1(int x)
{
  bar(x);
  bar(x + 1);
  /* { dg-final { scan-assembler-times "\ttmll\t%r15,3840" 1 { target { lp64 } } } } */
  /* { dg-final { scan-assembler-times "\ttml\t%r15,3968" 1 { target { ! lp64 } } } } */
}

__attribute__ ((target("stack-size=8192,stack-guard=0")))
void a0(int x)
{
  bar(x);
  bar(x + 1);
  /* { dg-final { scan-assembler-times "\ttmll\t%r15,7936" 1 { target { lp64 } } } } */
  /* { dg-final { scan-assembler-times "\ttml\t%r15,8064" 1 { target { ! lp64 } } } } */
}

void d(int x)
{
  bar(x);
  bar(x + 1);
}
