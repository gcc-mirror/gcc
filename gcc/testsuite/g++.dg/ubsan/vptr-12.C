// { dg-do run }
// { dg-shouldfail "ubsan" }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

struct MyClass
{
  virtual ~MyClass () {}
  virtual void Doit () {}
};

int
main ()
{
  MyClass *c = new MyClass;
  c->~MyClass ();
  c->Doit ();

  return 0;
}

// { dg-output "\[^\n\r]*vptr-12.C:16:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'MyClass'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object has invalid vptr" }
