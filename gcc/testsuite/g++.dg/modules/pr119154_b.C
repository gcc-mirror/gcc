// PR c++/119154
// { dg-module-do link }
// { dg-additional-options "-fmodules" }

void bar();
import foo;

int main() {
  bar();
}
