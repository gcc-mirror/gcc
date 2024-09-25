// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed ptolemy-bugs
#include <iostream>

class PTcl {
public:
	int dispatcher(int which,int argc,char** argv);
	// fns in the table
	int one(int argc, char** argv);
	int two(int argc, char** argv);
	int three(int argc, char** argv);
};

// An InterpFuncP is a pointer to an PTcl function that takes an argc-argv
// argument list and returns TCL_OK or TCL_ERROR.

typedef int (PTcl::*InterpFuncP)(int,char**);

struct InterpTableEntry {
	const char* name;
	InterpFuncP func;
};

// Here is the function table and dispatcher function.
// These macros define entries for the table

#define quote(x) #x
#define ENTRY(verb) { quote(verb), &PTcl::verb }

static InterpTableEntry funcTable[] = {
	ENTRY(one),
	ENTRY(two),
	ENTRY(three),
	{0, 0}
};

int PTcl::dispatcher(int which, int argc, char** argv) {
	return (this->*(funcTable[which].func))(argc, argv);
}

void printargs(char** argv) {
//	while (*argv) {
//		cout << " " << *argv++;
//	}
//	cout << "\n";
}

int PTcl::one(int, char** argv) {
  std::cout << "FAIL\n";
	printargs(argv);
	return 1;
}

int PTcl::two(int, char** argv) {
  std::cout << "PASS\n";
	printargs(argv);
	return 0;
}

int PTcl::three(int, char** argv) {
  std::cout << "FAIL\n";
	printargs(argv);
	return 1;
}

int main (int argc, char** argv) {
	PTcl obj;
	return obj.dispatcher(1,argc,argv);
}
