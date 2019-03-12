// { dg-additional-options -fmodules-ts }
import frob;

int foo (frob *p)
{
  return p->m;
}
