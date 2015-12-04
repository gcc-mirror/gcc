/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-mwarn-framesize=0" } */

#pragma GCC target("warn-framesize=1024")
void p1(void)
{
  char b[1025];
} /* { dg-warning "frame size" } */
#pragma GCC reset_options

#pragma GCC target("warn-framesize=0")
void p0(void)
{
  char b[1025];
}
#pragma GCC reset_options

__attribute__ ((target("warn-framesize=1024")))
void a1(void)
{
  char b[1025];
} /* { dg-warning "frame size" } */

__attribute__ ((target("warn-framesize=0")))
void a0(void)
{
  char b[1025];
}

void d(void)
{
  char b[1025];
}
