// PR c++/118961
// { dg-module-do link }
// { dg-require-effective-target lto }
// { dg-additional-options "-fmodules -flto -static -std=c++20" }
// { dg-skip-if "required hosted libstdc++ for string in lto-3_a.H" { ! hostedlib } }

import "lto-3_a.H";

int main() {
  std::string m_message;
}
