// Bug: default operator= is not being generated properly.
// PRMS Id: 3525
// Build don't link:

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
    e = ccEllipseUnit();	// gets bogus error - assignment not defined
}
