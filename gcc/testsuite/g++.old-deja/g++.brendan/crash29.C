// Build don't link: 
// GROUPS passed old-abort

union Value
{
	Value(){}
};

struct GlobalAddress
{// ERROR -  candidates .*
	GlobalAddress(Value *nvar){}// ERROR - .*
};

int
main()
{
	new GlobalAddress(Value());		// internal error occured here// ERROR -  no matching function .*
	//new GlobalAddress(new Value());	// This line is correct code
}
