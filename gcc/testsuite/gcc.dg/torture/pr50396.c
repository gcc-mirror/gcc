/* { dg-do run } */
/* { dg-add-options ieee } */

extern void abort (void);
typedef float vf128 __attribute__((vector_size(16)));
typedef float vf64 __attribute__((vector_size(8)));
int main()
{
#if !__FINITE_MATH_ONLY__
#if __FLT_HAS_QUIET_NAN__
  vf128 v = (vf128){ 0.f, 0.f, 0.f, 0.f };
  vf64 u = (vf64){ 0.f, 0.f };
  v = v / (vf128){ 0.f, 0.f, 0.f, 0.f };
  if (v[0] == v[0])
    abort ();
  u = u / (vf64){ 0.f, 0.f };
  if (u[0] == u[0])
    abort ();
#endif
#endif
  return 0;
}
