// { dg-do run { target ia64-*-* } }
// { dg-options "-fabi-version=0" }

extern "C" void abort ();

struct ConstructedObject {
  ConstructedObject() {};
  ~ConstructedObject() {};
  ConstructedObject(const ConstructedObject &from) {};
};

struct FrameworkObject {
  ConstructedObject action();
};

ConstructedObject FrameworkObject::action() {
  void *r32, *r33;

  asm("mov %0 = r32\nmov %1 = r33" : "=r"(r32), "=r"(r33) : );
  if (this != r33) {
    abort ();
  }
}

int main()
{
  FrameworkObject slawa;
  slawa.action();
  return 0;
}

