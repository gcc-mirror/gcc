// { dg-do run }
// { dg-options "-fno-omit-frame-pointer -fno-optimize-sibling-calls" }
// { dg-additional-options "-mno-omit-leaf-frame-pointer" { target { i?86-*-* x86_64-*-* } } }
// { dg-shouldfail "asan" }

int global[10];
void __attribute__((noinline)) call4(int i) { global[i+10]++; }
void __attribute__((noinline)) call3(int i) { call4(i); }
void __attribute__((noinline)) call2(int i) { call3(i); }
void __attribute__((noinline)) call1(int i) { call2(i); }
volatile int one = 1;
int main() {
  call1(one);
  return global[0];
}

// { dg-output "AddressSanitizer:? global-buffer-overflow.*(\n|\r\n|\r)" }
// { dg-output "    #0 0x\[0-9a-f\]+ (in \[^\n\r]*call4\[^\n\r]*|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "    #1 0x\[0-9a-f\]+ (in \[^\n\r]*call3\[^\n\r]*|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "    #2 0x\[0-9a-f\]+ (in \[^\n\r]*call2\[^\n\r]*|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "    #3 0x\[0-9a-f\]+ (in \[^\n\r]*call1\[^\n\r]*|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "    #4 0x\[0-9a-f\]+ (in \[^\n\r]*main\[^\n\r]*|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
