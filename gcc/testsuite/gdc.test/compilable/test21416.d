// https://github.com/dlang/dmd/issues/21416

struct S
{
	@disable this(ref S);
}

ref id(ref S s) => s;

S* p;
void foo(S s) { }

void main()
{
	S s;
	foo(__rvalue(id(s)));
}
