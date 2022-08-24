/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue -fno-stack-protector" } */

enum machine_mode
{
  FAKE_0,
  FAKE_1,
  FAKE_2,
  FAKE_3,
  FAKE_4,
  FAKE_5,
  NUM_MACHINE_MODES,
};

typedef int *rtx;
typedef long unsigned int size_t;
extern unsigned char mode_size[NUM_MACHINE_MODES];

extern rtx c_readstr (const char *, enum machine_mode);
extern rtx convert_to_mode (enum machine_mode, rtx, int);
extern rtx expand_mult (enum machine_mode, rtx, rtx, rtx, int);
extern rtx force_reg (enum machine_mode, rtx);
extern unsigned char mode_size_inline (enum machine_mode);
extern void *memset (void *__s, int __c, size_t __n);

rtx
builtin_memset_gen_str (void *data, long offset __attribute__ ((__unused__)),
			enum machine_mode mode)
{
  rtx target, coeff;
  size_t size;
  char *p;

  size = ((unsigned short) (__builtin_constant_p (mode)
			    ? mode_size_inline (mode) : mode_size[mode]));
  if (size == 1)
    return (rtx) data;

  p = ((char *) __builtin_alloca(sizeof (char) * (size)));
  memset (p, 1, size);
  coeff = c_readstr (p, mode);

  target = convert_to_mode (mode, (rtx) data, 1);
  target = expand_mult (mode, target, coeff, (rtx) 0, 1);
  return force_reg (mode, target);
}

/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue"  } } */
