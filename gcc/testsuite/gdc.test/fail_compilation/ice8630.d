auto map(alias func, R)(R r) { return r; }
typeof(v) foo(R)(R v) { return map!(p=>p)(v); }
void main() { foo([1]); }
