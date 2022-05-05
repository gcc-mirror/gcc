/*
TEST_OUTPUT:
---
fail_compilation/issue22820.d(138): Error: upper and lower bounds are needed to slice a pointer
fail_compilation/issue22820.d(138):        pointer `s1` points to an aggregate that defines an `opIndex`, perhaps you meant `(*s1)[]`
fail_compilation/issue22820.d(139): Error: upper and lower bounds are needed to slice a pointer
fail_compilation/issue22820.d(139):        pointer `s2` points to an aggregate that defines an `opSlice`, perhaps you meant `(*s2)[]`
fail_compilation/issue22820.d(140): Error: upper and lower bounds are needed to slice a pointer
fail_compilation/issue22820.d(140):        pointer `s3` points to an aggregate that defines an `opIndex`, perhaps you meant `(*s3)[]`
fail_compilation/issue22820.d(141): Error: upper and lower bounds are needed to slice a pointer
fail_compilation/issue22820.d(141):        pointer `cp` points to an aggregate that defines an `opIndex`, perhaps you meant `(*cp)[]`
fail_compilation/issue22820.d(142): Error: upper and lower bounds are needed to slice a pointer
fail_compilation/issue22820.d(142):        pointer `e` points to an aggregate that defines an `opIndex`, perhaps you meant `(*e)[]`
---
*/

#line 100

// normal functions
struct S1 {
	int[] opIndex() { return a; }
	int[] a;
}

// opSlice alternative
struct S2 {
	int[] opSlice() { return a; }
	int[] a;
}

// templates
struct S3 {
	int[] opIndex()() { return a; }
	int[] a;
}

class C {
	int[] opIndex()() { return a; }
	int[] a;
}

enum E : S1
{
	a = S1([1])
}

void main() {
	S1* s1 = new S1;
	S2* s2 = new S2;
	S3* s3 = new S3;
	C c = new C;
	C* cp = &c;
	E* e = new E;
	int* p;

	p = s1[].ptr;
	p = s2[].ptr;
	p = s3[].ptr;
	p = cp[].ptr;
	p = e[].ptr;

	p = (*s1)[].ptr;
	p = (*s2)[].ptr;
	p = (*s3)[].ptr;
	p = (*cp)[].ptr;
	p = (*e)[].ptr;
}
