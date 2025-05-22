// PR c++/120363
// { dg-module-do link }
// { dg-require-effective-target tls_runtime }
// { dg-add-options tls }
// { dg-additional-options "-fmodules" }

import M;

int main() {
  auto& instance = test::get_instance();
}
