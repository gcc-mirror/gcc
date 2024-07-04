/* Reduced from false positive in Linux kernel in sound/core/rawmidi.c,
   with sanitization removed to make it a true positive.

   Verify that we detect this (with default params).  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer -O2 -Wanalyzer-too-complex" } */
/* { dg-require-effective-target analyzer } */

typedef unsigned long __kernel_ulong_t;
typedef __kernel_ulong_t __kernel_size_t;
typedef __kernel_size_t size_t;
typedef unsigned int gfp_t;

extern unsigned long copy_from_user(void* to, const void* from, unsigned long n);

extern
__attribute__((__alloc_size__(1)))
__attribute__((__malloc__)) void*
kvzalloc(size_t size, gfp_t flags);

struct snd_rawmidi_params
{
  int stream;
  size_t buffer_size;
};

char *newbuf;

static int
resize_runtime_buffer(struct snd_rawmidi_params* params)
{
  /* No sanitization, so we should complain.  */
  
  newbuf = kvzalloc(params->buffer_size, /* { dg-warning "use of attacker-controlled value '\\*params.buffer_size' as allocation size without upper-bounds checking" "PR analyzer/112850" } */
		    (((gfp_t)(0x400u | 0x800u)) | ((gfp_t)0x40u) | ((gfp_t)0x80u)));
  if (!newbuf)
    return -12;
  return 0;
}

long
snd_rawmidi_ioctl(unsigned long arg)
{
  void* argp = (void*)arg;
  struct snd_rawmidi_params params;
  if (copy_from_user(&params, argp, sizeof(struct snd_rawmidi_params)))
    return -14;
  return resize_runtime_buffer(&params);
}
