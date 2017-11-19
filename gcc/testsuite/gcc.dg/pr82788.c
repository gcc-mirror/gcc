/* { dg-do run } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-probe-interval=10 --param stack-clash-protection-guard-size=12" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
int main() { int a[1442]; return 0;}
