void filter(R)(scope bool delegate(ref BAD!R) func) { }
void main() { filter(r => r); }

