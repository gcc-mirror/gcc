// { dg-do assemble  }
// GROUPS passed old-abort

union Value
{
	Value(){}
};

struct GlobalAddress
{// { dg-error "" }  candidates .*
	GlobalAddress(Value *nvar){}// { dg-error "" } .*
};

int
main()
{
	new GlobalAddress(Value());		// internal error occured here// { dg-error "" }  no matching function .*
	//new GlobalAddress(new Value());	// This line is correct code
}
