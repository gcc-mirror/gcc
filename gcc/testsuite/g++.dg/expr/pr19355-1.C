// PR c++/19355
// { dg-do compile }

typedef bool Boolean;
extern Boolean is_nil ();
void f(void)
{
  unsigned int ilen;
  if(!((ilen > 0 ? !is_nil () : 1))) {}
}

