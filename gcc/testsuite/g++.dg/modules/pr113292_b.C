// PR c++/113292
// { dg-additional-options "-fmodules-ts" }

import "pr113292_a.H";

// provide a definition of 'instance' so things link
thread_local test test::instance;

void instantiate() {
  auto& instance = test::get_instance();
  auto& t_instance = test_template<int>::get_instance();
  auto& tt_instance = test_template<int>::get_template_instance<double>();
};
