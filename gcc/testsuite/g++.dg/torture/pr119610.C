// { dg-do run }
// { dg-additional-options "-fstack-protector-strong" { target fstack_protector } }
// { dg-additional-options "-fstack-clash-protection" { target supports_stack_clash_protection } }

int *ptr;
void foo() {
  int c[1024*128];
  ptr = c;
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
