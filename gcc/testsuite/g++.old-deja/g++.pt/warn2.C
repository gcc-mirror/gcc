// { dg-do assemble  }
// { dg-options "-Wall" }
// Origin: Jeroen@MMR.be

template <typename T>
void f()
{
  for(;;)
    for(;;)
      goto a;

 a:
  ;
}

void g()
{
  f<long>();
}
