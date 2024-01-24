/* Reduced from false positive in Linux kernel
   in drivers/char/ipmi/ipmi_devintf.c.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer -O2 -Wno-attributes" } */
/* { dg-require-effective-target analyzer } */

typedef __SIZE_TYPE__ size_t;
extern void
__check_object_size(const void* ptr, unsigned long n);

extern unsigned long
copy_from_user(void*, const void*, unsigned long);

__attribute__((__always_inline__)) unsigned long
call_copy_from_user(void* to, const void* from, unsigned long n)
{
  __check_object_size(to, n);
  n = copy_from_user(to, from, n); /* { dg-bogus "use of attacker-controlled value as size without upper-bounds checking" } */
  return n;
}
struct ipmi_msg
{
  unsigned short data_len;
  unsigned char* data;
};

static int
handle_send_req(struct ipmi_msg* msg)
{
  char buf[273];
  if (msg->data_len > 272) {
    return -90;
  }
  if (call_copy_from_user(buf, msg->data, msg->data_len)) {
    return -14;
  }
  return 0;
}
long
ipmi_ioctl(void* arg)
{
  struct ipmi_msg msg;
  if (call_copy_from_user(&msg, arg, sizeof(msg))) {
    return -14;
  }

  return handle_send_req(&msg);
}
