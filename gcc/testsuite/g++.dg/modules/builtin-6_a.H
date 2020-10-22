// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

extern "C" {
  
static double sin (double)
{
  // extra small angle approximation :)
  return 0.0;
}

}
