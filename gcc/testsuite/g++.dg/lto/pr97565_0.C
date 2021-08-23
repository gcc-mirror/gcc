// { dg-lto-do link }
// { dg-lto-options { "-O -flto -fipa-pta" } }

extern "C" void abort(void)
{
  abort();
}
