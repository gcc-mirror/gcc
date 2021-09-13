/* { dg-do compile } */

#pragma GCC push_options
#pragma GCC target("prefer-vector-width=512")

int
__attribute__((target("prefer-vector-width=none")))
main()
{
  return 0;
}
