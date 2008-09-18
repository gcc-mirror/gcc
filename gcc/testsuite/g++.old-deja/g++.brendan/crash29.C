// { dg-do assemble  }
// GROUPS passed old-abort

union Value
{
	Value(){}
};

struct GlobalAddress
{// { dg-message "note" }
	GlobalAddress(Value *nvar){} // { dg-message "candidates" }
};

int
main()
{
	new GlobalAddress(Value());		// internal error occured here// { dg-error "no matching" }
	//new GlobalAddress(new Value());	// This line is correct code
}
