// g++ 1.36.1 bug 900209_01

// g++ implicitly casts values whose types are "void*" to other pointer
// types (just as the language rules for C permit).  Such implicit
// conversions are not allowed by the Cfront 2.0 Reference Manual
// however.

// Cfront 2.0 passes this test.

// keywords: void pointers, type conversions, pointer type conversions

void * void_pointer_object;
char * char_pointer_object;

void global_function_0 ()
{
  char_pointer_object = void_pointer_object;	// ERROR - 
}

int main () { return 0; }
