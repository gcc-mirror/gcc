// { dg-do assemble  }
// Bug: default operator= is not being generated properly.
// PRMS Id: 3525

struct ccUnwind 
{
    ccUnwind& operator = (const ccUnwind&);
};

struct ccEllipseUnit
{ 
    ccEllipseUnit () {}
};

struct ccEllipse : ccUnwind  
{   
    ccEllipse ();
    ccEllipse (const ccEllipseUnit&);

};

void foo ()
{
    ccEllipse e;
    e = ccEllipseUnit();	// { dg-bogus "" } assignment not defined
}
