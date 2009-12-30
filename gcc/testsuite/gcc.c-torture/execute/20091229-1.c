long long foo(long long v) { return v / -0x080000000LL; }
void main() { if (foo(0x080000000LL) != -1) abort(); exit (0); }
