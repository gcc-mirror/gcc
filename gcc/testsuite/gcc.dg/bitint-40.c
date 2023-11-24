/* PR middle-end/112668 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -fnon-call-exceptions" } */

#if __BITINT_MAXWIDTH__ >= 156
struct T156 { _BitInt(156) a : 2; unsigned _BitInt(156) b : 135; _BitInt(156) c : 2; };
extern void foo156 (struct T156 *);

unsigned _BitInt(156)
bar156 (int i)
{
  struct T156 r156[12];
  foo156 (&r156[0]);
  return r156[i].b;
}
#endif

#if __BITINT_MAXWIDTH__ >= 495
struct T495 { _BitInt(495) a : 2; unsigned _BitInt(495) b : 471; _BitInt(495) c : 2; };
extern void foo495 (struct T495 *r495);

unsigned _BitInt(495)
bar495 (int i)
{
  struct T495 r495[12];
  foo495 (r495);
  return r495[i].b;
}
#endif
