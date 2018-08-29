// { dg-do assemble  }
// { dg-additional-options "-Wno-return-type" }
// GROUPS passed initialization
class Vector {
        double  *v;
        int             size;

public:
        Vector(int n);
        ~Vector();
};

exmpl() { Vector x(8)[16]; }// { dg-error "" } .*

