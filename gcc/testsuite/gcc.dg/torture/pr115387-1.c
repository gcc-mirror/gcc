/* Test there is no ICE when compile.  */
/* { dg-do compile } */

#define PRINTF_CHK 0x34

typedef __UINTPTR_TYPE__ uintptr_t;

struct __printf_buffer {
  char *write_ptr;
  int status;
};

extern void __printf_buffer_init_end (struct __printf_buffer *, char *, char *);

void
test (char *string, unsigned long maxlen, unsigned mode_flags)
{
  struct __printf_buffer buf;

  if ((mode_flags & PRINTF_CHK) != 0)
    {
      string[0] = '\0';
      uintptr_t end;

      if (__builtin_add_overflow ((uintptr_t) string, maxlen, &end))
	end = -1;

      __printf_buffer_init_end (&buf, string, (char *) end);
    }
  else
    __printf_buffer_init_end (&buf, string, (char *) ~(uintptr_t) 0);

  *buf.write_ptr = '\0';
}
