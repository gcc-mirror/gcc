/* { dg-options "-O2 -std=c++11 -fdump-ipa-inline"  } */
#include <new>

class EmbeddedObject {
public:
  virtual int val() { return 2; }
};

class Container {
  alignas(EmbeddedObject) char buffer[sizeof(EmbeddedObject)];
public:
  EmbeddedObject *obj() { return (EmbeddedObject*)buffer; }
  Container() { new (buffer) EmbeddedObject(); }
};

Container o;

int main()
{
  __builtin_printf("%d\n", o.obj()->val());
}
/* { dg-final { scan-ipa-dump-not "__builtin_unreachable"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
