// { dg-additional-options "-fmodules-ts" }
// { dg-prune-output "not writing module" }

export module M;
int main() {}  // { dg-error "attach" }
