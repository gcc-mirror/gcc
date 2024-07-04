/* PR 112347 
   { dg-do compile }
   { dg-options "-Walloc-size" }
 * */

// Test that various types without size do not crash with -Walloc-size

int * mallocx(unsigned long) __attribute__((malloc)) __attribute__((alloc_size(1)));
void test_oom(void) { void *a_ = mallocx(1); }

void parse_args(char (**child_args_ptr_ptr)[]) {
  *child_args_ptr_ptr = __builtin_calloc(1, sizeof(char));
}


