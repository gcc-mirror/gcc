// g++ 1.37.1 bug 900519_05

// g++ fails to allow the use of function-reference types.

// cfront 2.0 passes this test.

// keywords: function types, reference types

typedef void (func_type) (int, int);
typedef func_type& func_ref_type;		

void function (int arg1, int arg2)
{
}

func_type& global_func_ref1 = function;		

int main () { return 0; }
