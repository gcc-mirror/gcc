/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto}} } */

extern void link_error (void);
short *p __attribute__((used));
int i __attribute__((used));

int main()
{
  if (i == 0)
    return;

  *p = 0;

  if (i == 0)
    link_error ();

  return 0;
}
