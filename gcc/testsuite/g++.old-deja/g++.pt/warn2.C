// Build don't link:
// Special g++ Options: -Wall
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
