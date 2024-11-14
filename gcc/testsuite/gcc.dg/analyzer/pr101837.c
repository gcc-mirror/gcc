/* { dg-additional-options "-O3 -fsanitize=undefined -fno-malloc-dce" } */

void memory_exhausted();
void memcheck(void *ptr) {
  if (ptr) /* { dg-warning "leak" } */
    memory_exhausted();
}

int emalloc(int size) { memcheck(__builtin_malloc(size)); } /* { dg-message "allocated here" } */
int main() { int max_envvar_len = emalloc(max_envvar_len + 1); } /* { dg-message "use of uninitialized value 'max_envvar_len'" } */
