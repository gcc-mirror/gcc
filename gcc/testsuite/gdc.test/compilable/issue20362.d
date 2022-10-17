void main() {
    string str;
    stringify((chars) {str ~= chars; });
}

void stringify(scope void delegate(scope const char[]) sink) {
    sink("oops");
}
