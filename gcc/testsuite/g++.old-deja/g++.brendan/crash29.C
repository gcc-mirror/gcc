// { dg-do assemble  }
// GROUPS passed old-abort

union Value
{
	Value(){}
};

struct GlobalAddress		// { dg-message "note" }
{
	GlobalAddress(Value *nvar){} // { dg-message "note" }
};

int
main()
{
	new GlobalAddress(Value());		// internal error occured here// { dg-error "no matching" }
	// { dg-message "candidate" "candidate note" { target *-*-* } 17 }
	//new GlobalAddress(new Value());	// This line is correct code
}
