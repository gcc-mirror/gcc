typedef __SIZE_TYPE__ size_t;

#define __user

extern int copy_from_user(void *to, const void __user *from, long n)
  __attribute__((access (write_only, 1, 3),
		 access (read_only, 2, 3)
		 ));

#define   EFAULT          14
#define   EINVAL          22

/* Taken from Linux: fs/binfmt_misc.c (GPL-2.0-only).  */

int parse_command(const char __user *buffer, size_t count)
{
	char s[4];

	if (count > 3)
		return -EINVAL;
	if (copy_from_user(s, buffer, count))
		return -EFAULT;
	if (!count)
		return 0;
	if (s[count - 1] == '\n') /* { dg-bogus "uninit" } */
		count--;
	if (count == 1 && s[0] == '0') /* { dg-bogus "uninit" } */
		return 1;
	if (count == 1 && s[0] == '1') /* { dg-bogus "uninit" } */
		return 2;
	if (count == 2 && s[0] == '-' && s[1] == '1') /* { dg-bogus "uninit" } */
		return 3;
	return -EINVAL;
}

/* Not using return value from copy_from_user.  */

int test_2 (const char __user *buffer, size_t count)
{
  char s[4];
  if (count > 3)
    return -EINVAL;
  copy_from_user(s, buffer, count);
  return 0;  
}
