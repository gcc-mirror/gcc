// Test that the destructor for a temporary passed by value isn't run
// until end of full-expression, as per [class.copy]:

// Whenever a temporary class object is copied using a copy  constructor,
// and  this  object  and  the copy have the same cv-unqualified type, an
// implementation is permitted to treat the original and the copy as  two
// different  ways of referring to the same object and not perform a copy
// at all, even if the class copy constructor  or  destructor  have  side
// effects....                                       In these cases,  the
// object  is  destroyed  at the later of times when the original and the
// copy would have been destroyed without the optimization.

// Here, the temporary would be destroyed later than the parm, so either we
// must suppress the optimization in this case or destroy value parms in the
// caller.

// Split into pieces for binary compatibility testing October 2002

extern void elide1_x (void);

int
main ()
{
  elide1_x ();
}
