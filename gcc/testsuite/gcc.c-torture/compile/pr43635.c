/* { dg-require-effective-target untyped_assembly } */
extern void d (void);

void (*foo (void)) (float)
{
  void (*(*x) (void)) (float) = d;
  return (*x) ();
}
