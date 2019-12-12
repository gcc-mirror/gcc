/* { dg-do run } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-probe-interval=10 --param stack-clash-protection-guard-size=12" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "AArch64 does not support this interval." { aarch64*-*-* } } */
int main() { int a[1442]; return 0;}
