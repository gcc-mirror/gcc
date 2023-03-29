/* Verify no errors for 2, 6 and 14 nops before local entry on ELFv2.  */

extern int a;

__attribute__ ((noipa, patchable_function_entry (2, 2)))
int test1 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (4, 2)))
int test2 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (6, 6)))
int test3 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (8, 6)))
int test4 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (16, 6)))
int test5 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (14, 14)))
int test6 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (28, 14)))
int test7 (int b) {
  return a + b;
}

__attribute__ ((noipa, patchable_function_entry (64, 14)))
int test8 (int b) {
  return a + b;
}
