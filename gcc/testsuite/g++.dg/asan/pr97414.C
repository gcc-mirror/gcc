/* PR sanitizer/97414 */
/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=1:halt_on_error=1,detect_stack_use_after_return=1" } */
/* { dg-options "-fsanitize=address,pointer-compare,pointer-subtract" } */

[[gnu::noinline]] auto pointer_diff(const int *begin, const int *end) {
  return end - begin;
}

int main() {
  constexpr auto size = (2048 / sizeof(int)) + 1;

  auto buf = new int[size];
  auto end = buf + size;
  pointer_diff(end, buf);
  delete[] buf;

  return 0;
}
