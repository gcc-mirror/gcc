// PR c++/113292
// { dg-module-do link }
// { dg-require-effective-target tls_runtime }
// { dg-add-options tls }
// { dg-additional-options "-fmodules-ts" }

import "pr113292_a.H";

int main() {
  auto& instance = test::get_instance();
  auto& t_instance = test_template<int>::get_instance();
  auto& tt_instance = test_template<int>::get_template_instance<double>();
}
