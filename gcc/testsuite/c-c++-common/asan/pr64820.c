/* { dg-do run } */
/* { dg-require-effective-target fstack_protector } */
/* { dg-options "-fstack-protector-strong" } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_stack_use_after_return=1" } */
/* { dg-shouldfail "asan" } */

__attribute__((noinline))
char *Ident(char *x) {
  return x;
}

__attribute__((noinline))
char *Func1() {
  char local[1 << 12];
  return Ident(local);
}

__attribute__((noinline))
void Func2(char *x) {
  *x = 1;
}
int main(int argc, char **argv) {
  Func2(Func1());
  return 0;
}

/* { dg-output "AddressSanitizer: stack-use-after-return on address 0x\[0-9a-f\]+\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "WRITE of size 1 at .* thread T0.*" } */
/* { dg-output "    #0.*(Func2)?.*pr64820.(c:21)?.*" } */
/* { dg-output "is located in stack of thread T0 at offset.*" } */
/* { dg-output "\'local\' \\(line 14\\) <== Memory access at offset 32 is inside this variable" } */
