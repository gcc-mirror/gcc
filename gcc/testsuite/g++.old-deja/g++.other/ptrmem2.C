class cow {
public:
  void moo (char *);
};

void f()
{
  cow* c;

  void (cow::*fp0)(char*) = &cow::moo;  // OK
  void (cow::*fp1)(int) = &cow::moo;    // ERROR - conversion
  int (cow::*fp2)(char*) = &cow::moo;   // ERROR - conversion
  int (cow::*fp3)(char*, void*) = fp2;  // ERROR - conversion
  int (cow::*fp4)(double) = (int (cow::*)(double)) fp2; // OK
}
