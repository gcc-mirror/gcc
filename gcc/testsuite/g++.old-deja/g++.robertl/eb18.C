// { dg-do assemble  }
class C { };

void foo()
{
    C c;
    void * v = static_cast<void *>(c);  // { dg-error "" } illegal cast
}
