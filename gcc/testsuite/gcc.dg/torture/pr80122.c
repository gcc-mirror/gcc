/* { dg-do run } */

#define __GNU_ALWAYS_INLINE inline __attribute__(( __always_inline__))

#define DEVT_ALL    0

#define CMD_ABI_DEVICES 100

static __GNU_ALWAYS_INLINE int
send_msg_to_gm_w_dev_t(int msg_type, unsigned int dev_msg_type,
		       int devt, ...)
{
  char s[256];
  int nArgs = __builtin_va_arg_pack_len();
  if (nArgs != 2)
    __builtin_abort ();
  __builtin_sprintf (s, "%d", __builtin_va_arg_pack ());
  if (__builtin_strcmp (s, "99") != 0)
    __builtin_abort ();
  /* do something with nArgs and ... */ 
  return 0;
}

static __GNU_ALWAYS_INLINE int
send_msg_to_gm(int msg_type, unsigned int dev_msg_type,
	       ...)
{
  int nArgs = __builtin_va_arg_pack_len();
  if (nArgs != 2)
    __builtin_abort ();
  return send_msg_to_gm_w_dev_t(msg_type, dev_msg_type,
				DEVT_ALL, __builtin_va_arg_pack()); 
}

static __GNU_ALWAYS_INLINE int
send_enable(unsigned int dev_msg_type, ...)
{
  int nArgs = __builtin_va_arg_pack_len();
  if (nArgs != 2)
    __builtin_abort ();
  return send_msg_to_gm(CMD_ABI_DEVICES, dev_msg_type,  __builtin_va_arg_pack());
}

int 
main(void)
{
  int mode = 99;

  send_enable(1, mode, sizeof(mode));

  return 0;
}
