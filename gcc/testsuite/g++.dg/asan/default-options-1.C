// { dg-do run }

const char *kAsanDefaultOptions="verbosity=1 foo=bar";

extern "C"
__attribute__((no_sanitize_address))
const char *__asan_default_options() {
  return kAsanDefaultOptions;
}

int main() {
  return 0;
}

// { dg-output "WARNING: found 1 unrecognized flag\\(s\\):(\n|\r\n|\r).*foo(\n|\r\n|\r)" }
