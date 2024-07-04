// PR c++/110730
// { dg-additional-options -fmodules-ts }

import "namespace-6_a.H";

int main() { std::filesystem::current_path(); }
// { dg-final { scan-assembler _ZNSt10filesystem12current_pathB5cxx11B3fooEv } }
