// { dg-do assemble  }
// GROUPS passed old-abort
class X {
public:
void doit();
};

X::::doit()// { dg-error "" }  (syntax|parse) error.*
{
}
