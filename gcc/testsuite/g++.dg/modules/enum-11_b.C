// PR c++/102600
// { dg-additional-options -fmodules-ts }

import "enum-11_a.H";

void push(byte) {}
void write(char v) { push(static_cast<byte>(v)); }
int main() { write(char{}); }
