/* Verify that arg const 4 to 2nd call is const and not reg r0 which is
   ret value of first call.
   Extracted from BPF selftest attach_probe.c  */

/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v4" } */

extern int bpf_copy_from_user_str(int dst__sz);

_Bool verify_sleepable_user_copy_str(void)
{
 int ret;
 char data_short_pad[4];

 ret = bpf_copy_from_user_str(sizeof(data_short_pad));

 if (ret != 4)
  return false;

 ret = bpf_copy_from_user_str(sizeof(data_short_pad));

 if (ret != 4)
  return false;

  return true;
}

/* Original generated code
	r1 = 4
	call	bpf_copy_from_user_str
	r1 = (s32) r0
    ...
	call	bpf_copy_from_user_str

   Now generated
	r1 = 4
	call	bpf_copy_from_user_str
    ...
	r1 = 4
	call	bpf_copy_from_user_str  */

/* { dg-final { scan-assembler-times {r1 = 4} 2 } } */
