// g++ 1.37.1 bug 900325_01

// g++ fails to generate errors for attempts to declare a formal argument to
// be of a void type.

// keywords: formal parameter, void type

typedef void __void;
typedef __void Void;

void function0 (void arg1) {	// ERROR - missed
}

void function1 (Void arg1) {	// ERROR - missed
}

int main () { return 0; }
