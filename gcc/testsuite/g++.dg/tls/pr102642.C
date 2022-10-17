// PR c++/102642
// { dg-do compile { target c++11 } }

thread_local int *z;		// { dg-message "previous declaration" }

void
foo ()
{
  extern thread_local int z;	// { dg-error "conflicting declaration" }
}
