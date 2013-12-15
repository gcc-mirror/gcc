/* Test for cross x86_64<->w64 abi va_list calls.  */
/* { dg-do run { target i?86-*-linux* x86_64-*-linux* } } */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -fno-builtin -maccumulate-outgoing-args" } */
/* { dg-additional-sources "vaarg-4b.c" } */

extern __SIZE_TYPE__ __attribute__ ((sysv_abi)) strlen (const char *);
extern int __attribute__ ((sysv_abi)) sprintf (char *,const char *, ...);
extern void __attribute__ ((sysv_abi)) abort (void);

extern void do_cpy (char *, ...);

int __attribute__ ((sysv_abi))
main ()
{
  char s[256];

  do_cpy (s, "1","2","3","4", "5", "6", "7", "");

  if (s[0] != '1' || s[1] !='2' || s[2] != '3' || s[3] != '4'
      || s[4] != '5' || s[5] != '6' || s[6] != '7' || s[7] != 0)
    abort ();

  return 0;
}
