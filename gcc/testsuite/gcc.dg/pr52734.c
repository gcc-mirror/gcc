/* { dg-do run } */
/* { dg-options "-O2" } */

int bbb = 0;

int __attribute__((noinline,noclone)) aaa(void)
{
    ++bbb;
    return 0;
}

int __attribute__((noinline,noclone)) ccc(void)
{
  int ddd;
  /* bbb == 0 */
  if (aaa())
    return bbb;

  /* bbb == 1 */
  ddd = bbb;
  /* bbb == ddd == 1 */
  if (aaa ())
    return 0;
  /* bbb == 2, ddd == 1 */

  return ddd;
}

int main(void)
{
    if (ccc() != 1)
	__builtin_abort();
    return 0;
}

