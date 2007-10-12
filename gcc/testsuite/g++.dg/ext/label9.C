// PR c++/32121
// { dg-do compile }

int f (void)
{
  while (1)
    __label__ a;	// { dg-error "not at the beginning" }
  for (;;)
    __label__ b;	// { dg-error "not at the beginning" }
}
