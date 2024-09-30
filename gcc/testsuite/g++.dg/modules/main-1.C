// { dg-additional-options "-fmodules-ts" }

export module M;
int main() {}  // { dg-error "attach" }
