// { dg-do run { target aarch64_sve_hw } }
// { dg-additional-options "-fstack-protector-strong" { target fstack_protector } }
// { dg-additional-options "-fstack-clash-protection" { target supports_stack_clash_protection } }

void *a_ptr, *b_ptr;
void foo() {
  __SVInt32_t a;
  int b[1024*128];
  a_ptr = &a;
  b_ptr = b;
  throw 1;
}
int main()
{
  try {
    foo();
  } catch(int x) {
    return 0;
  }
}
