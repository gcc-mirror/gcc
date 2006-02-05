// { dg-do run  }
// PRMS Id: 6000
// Bug: g++ gets confused trying to build up a reference to a cast.

class String {
protected:
  char *cp;
public:
  String(char *incp);
  String(const String &constStringRef);
  virtual void virtualFn1(void) const {;}
};

String::String(char *incp)
{
  cp = incp;
}

String::String(const String &constStringRef)
{
// Right here, do an 'info args', and look at the virtual function table
// pointer: typically junk! Calling the function through that table could
// do anything, since we're really leaping off into the void. This example
// goes down with 'SIGBUS', but I've seen 'SIGSEGV' too, and 'SIGILL' is
// possible.

  cp = constStringRef.cp;
  constStringRef.virtualFn1();
}

void foofun(String string)
{
  ;
}

class Class1 {
public:
  Class1(const String & constStringRef);
};

Class1 :: Class1 (const String & constStringRef)
{
// If instead of calling the function 'foofun()' here, we just assign
// 'constStringRef' to a local variable, then the vptr is typically == 0!

  foofun(String(constStringRef));
}

int main(void)
{
  Class1 *class1 = new Class1((char*) "Hi!");
}
