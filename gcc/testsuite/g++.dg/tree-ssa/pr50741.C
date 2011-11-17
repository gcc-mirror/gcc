/* { dg-do compile } */
/* { dg-options "-O2 -g" } */
/* PR middle-end/50741 */

struct PublishLo
{
  const char *functionName;
  ~PublishLo();
};
struct A { A(); };
A::A()
{
  static PublishLo _rL_53 = {__FUNCTION__};
}
