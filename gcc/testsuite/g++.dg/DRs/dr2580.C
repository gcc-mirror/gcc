// DR 2580 - Undefined behavior with #line
// { dg-do preprocess }

#line 630 "foobar.h"
#line 6 "dr2580.C"
#line 0				// { dg-error "line number out of range" }
#line 8
#line 2147483648		// { dg-error "line number out of range" }
#line 10
#line 4294967295		// { dg-error "line number out of range" }
#line 12
#line 12345678912345		// { dg-error "line number out of range" }
#line 14
#line 15 ""
#line 16 "foobar.h"
#line 17 "/a/b/c/baz.h"
#line 18 "dr2580.C"
#line 0 "dr2580.C"		// { dg-error "line number out of range" }
#line 20
#line 2147483648 "dr2580.C"	// { dg-error "line number out of range" }
#line 22
#line 4294967295 "dr2580.C"	// { dg-error "line number out of range" }
#line 24
#line 12345678912345 "dr2580.C"	// { dg-error "line number out of range" }
#line 26
#line 27 1			// { dg-error "'1' is not a valid filename" }
#line 28
#line 29 foo bar baz		// { dg-error "'foo' is not a valid filename" }
#line 30
#line 31 "dr2580.C" 1		// { dg-error "extra tokens at end of '#line' directive" }
#line 32
#line 33 "dr2580.C" foo bar baz	// { dg-error "extra tokens at end of '#line' directive" }
#define A 35
#line A
#define B 0
#line B				// { dg-error "line number out of range" }
#line 38
#define C 2147483648
#line C				// { dg-error "line number out of range" }
#line 41
#define D 4294967295
#line D				// { dg-error "line number out of range" }
#line 44
#define E 12345678912345
#line E				// { dg-error "line number out of range" }
#line 47
#define F 49 "dr2580.C"
#line F
#define G 0 "dr2580.C"
#line G				// { dg-error "line number out of range" }
#line 52 "dr2580.C"
#define H 2147483647 "dr2580.C"
#line H				// { dg-error "line number out of range" "" { target c++98_only } }
#line 55
#define I 2147483648 "dr2580.C"
#line I				// { dg-error "line number out of range" }
#line 58
#define J 4294967295 "dr2580.C"
#line J				// { dg-error "line number out of range" }
#line 61
#define K 12345678912345 "dr2580.C"
#line K				// { dg-error "line number out of range" }
#line 64
#define M 7 0
#line M				// { dg-error "'0' is not a valid filename" }
#line 67
#define N 90 foo bar baz
#line N				// { dg-error "'foo' is not a valid filename" }
#line 70
#define O 75 "dr2580.C" 2
#line O				// { dg-error "extra tokens at end of '#line' directive" }
#line 73
#define P 90 "dr2580.C" foo bar baz
#line P				// { dg-error "extra tokens at end of '#line' directive" }
#line 76
#line -5			// { dg-error "'-' after '#line' is not a positive integer" }
#line 78
#line -7 "dr2580.C"		// { dg-error "'-' after '#line' is not a positive integer" }
#line 80
#line 32767
#line 82
#line 32768			// { dg-error "line number out of range" "" { target c++98_only } }
#line 84
#line 32767 "dr2580.C"
#line 86
#line 32768 "dr2580.C"		// { dg-error "line number out of range" "" { target c++98_only } }
#line 88
