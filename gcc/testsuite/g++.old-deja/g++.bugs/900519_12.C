// g++ 1.37.1 bug 900519_12

// The following erroneous code causes g++ to segfault.

// cfront 2.0 passes this test.

// keywords: segfault, typedef, pointer type, function type

typedef eek void (*)();		// ERROR - 

int main () { return 0; }
