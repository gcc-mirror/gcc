// { dg-do assemble  }
// GROUPS passed ptolemy-bugs
class Tcl_Interp;

class PTcl {
public:
	PTcl(Tcl_Interp* interp = 0);
	~PTcl();
	int alias(int argc,char** argv);
};

typedef int (PTcl::*InterpFuncP)(int,char**);

struct InterpTableEntry {
	const char* name;
	InterpFuncP func;
};

 
static InterpTableEntry funcTable[] = {
	{ "alias" , &PTcl::alias  } ,
	0, 0
};
