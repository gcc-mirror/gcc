/* PR rtl-optimization/44469 */
/* Testcase by Siarhei Siamashka <siarhei.siamashka@gmail.com> */

int a (int *t, const char *p)
{
  if (*t == 0)
    {
    }
  else if (*t == 1)
    {
      p = (const char *)t;
    }
  else
    __builtin_unreachable();
  if (p[0])
    return 0;
  return 1;
}
