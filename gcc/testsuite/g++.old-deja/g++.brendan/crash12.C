// { dg-do assemble  }
// GROUPS passed old-abort
class X
{
        int i;
public:
        X(int j);
}

X *x = new X[10]();// { dg-error "" } .*
