// Build don't link: 
// GROUPS passed old-abort

union Value
{
	Value(){}
};

struct GlobalAddress
{
	GlobalAddress(Value *nvar){}// ERROR - .*
};// ERROR -  candidates .*

int
main()
{
	new GlobalAddress(Value());		// internal error occured here// ERROR -  no matching function .*
	//new GlobalAddress(new Value());	// This line is correct code
}
