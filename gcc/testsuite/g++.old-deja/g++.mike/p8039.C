// { dg-do assemble  }
// prms-id: 8039

class C {
public:
  int func ();
};

extern void bar(int*);

int main()
{
  int (C::*mfp)() = &C::func;
  bar((int*)mfp);		// { dg-error "" } no clear semantics
}
