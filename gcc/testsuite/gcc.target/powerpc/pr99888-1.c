/* Verify no errors for different nops after local entry on ELFv2.  */

extern int a;

__attribute__ ((noipa, patchable_function_entry (1, 0)))
int test1 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (2, 0)))
int test2 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (3, 0)))
int test3 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (4, 0)))
int test4 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (5, 0)))
int test5 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (6, 0)))
int test6 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (7, 0)))
int test7 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (8, 0)))
int test8 (int b) {
  return a + b;
}
