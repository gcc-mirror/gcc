// PR c++/118961
// { dg-module-do link }
// { dg-require-effective-target lto }
// { dg-additional-options "-fmodules -flto -std=c++20" }

import "lto-2_a.H";
int main() {
  foo();
}
