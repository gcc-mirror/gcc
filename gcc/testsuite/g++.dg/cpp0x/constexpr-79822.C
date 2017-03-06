// PR c++/79822
// { dg-do compile }
// { dg-options "" }

bool
foo ()
{
  bool a = ({ }) && false;		// { dg-error "could not convert" }
  bool b = ({ ; }) && false;		// { dg-error "could not convert" }
  bool c = ({ (void) 1; }) && false;	// { dg-error "could not convert" }
  return a && b && c;
}
