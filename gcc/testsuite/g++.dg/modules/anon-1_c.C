// { dg-additional-options -fmodules-ts }

import namer;
import anon;

int main ()
{
  foo obj;
  int *ip = &get_int (obj);
  float *fp = &get_float (obj);

  return !((void *)ip == (void *)fp);
}
