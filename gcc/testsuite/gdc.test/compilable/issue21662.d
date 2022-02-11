struct S { @disable this(); }
extern __gshared S a;
extern S[2] b;
void main() {}
