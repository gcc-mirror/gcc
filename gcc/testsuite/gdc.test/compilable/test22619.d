// https://issues.dlang.org/show_bug.cgi?id=22619

struct W1 {
	int x;
	this(ref inout W1 rhs) inout { this.x = rhs.x; }
}

inout(W1) f(inout W1 x) { return x; }
void g(W1 x) {
	auto r = f(x);
}
