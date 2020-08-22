/* PR middle-end/95052 */
/* { dg-do compile } */
/* { dg-options "-Os -mtune=skylake" } */
/* Verify we don't waste almost 2 megabytes of .rodata.  */
/* { dg-final { scan-assembler-not "\.zero\t1048\[0-9]\[0-9]\[0-9]" } } */
extern void foo (char *, unsigned);

int
main ()
{
  char str[1024 * 1024] =
    "fooiuhluhpiuhliuhliyfyukyfklyugkiuhpoipoipoipoipoipoipoipoipoipoipoipoipoimipoipiuhoulouihnliuhl";
  char arr[1024 * 1024] =
    { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 6, 2, 3,
      4, 5, 6, 7, 8, 9, 0, 3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6,
      7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 };
  foo (str, sizeof (str));
  foo (arr, sizeof (arr));
  return 0;
}
