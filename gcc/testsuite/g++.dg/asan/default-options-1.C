// { dg-do run }

const char *kAsanDefaultOptions="verbosity=1 foo=bar";

extern "C"
__attribute__((no_address_safety_analysis))
const char *__asan_default_options() {
  return kAsanDefaultOptions;
}

int main() {
  return 0;
}

// { dg-output "Using the defaults from __asan_default_options:.* foo=bar.*(\n|\r\n|\r)" }
