/* Test for cross x86_64<->w64 abi va_list calls.  */
/* { dg-do run } */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -fno-builtin -maccumulate-outgoing-args" } */
/* { dg-additional-sources "vaarg-5b.c" } */

extern void __attribute__ ((sysv_abi)) abort (void);
extern int fct2 (int, ...);

#define SZ_ARGS	1ll,2ll,3ll,4ll,5ll,6ll,7ll,0ll

int __attribute__ ((sysv_abi))
main()
{
  if (fct2 (-1, SZ_ARGS) != 0)
    abort ();
  return 0;
}
