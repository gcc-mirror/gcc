/* { dg-do compile } */
/* { dg-options "-O2 -Winline --param large-stack-frame=10 --param large-stack-frame-growth=2 -fgnu89-inline" } */

int a,b;
void test(char *);
static inline
int aa (void)
{
  char t[10];
  test(t);
}
static inline
int bb (void) /* { dg-warning "large-stack-frame" "" } */
{
  char t[100];
  test(t);
}

int
t()
{
  if (a)
    aa();
  if (b)
    bb(); 			/* { dg-message "called from here" } */
}
