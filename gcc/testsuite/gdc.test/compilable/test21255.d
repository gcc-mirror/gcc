module test21255;

void decodeA()(int i) { }
void decodeB()(string s) { }

alias decode = decodeA;
alias decode = decodeB;

void foo(alias A)() { A(1); A("hello"); }

void main() { foo!decode; }
