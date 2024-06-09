/* { dg-do compile } */
/* { dg-additional-options "-O2 -fstack-protector-strong -fstack-clash-protection" } */
void add_key(const void *payload);
void act_keyctl_test(void) {
  char buf[1030 * 1024];
  int i = 0;
  for (i = 0; i < sizeof(buf); i++)
  {
    add_key(buf);
  }
}
