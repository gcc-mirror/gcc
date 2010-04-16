/* { dg-do compile } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb1_ok } */

int foo(char ch)
{
  switch (ch) {
    case '-':
    case '?':
    case '/':
    case 99:
        return 1;
    default:
        return 0;
  }
}

/* { dg-final { scan-assembler-times "cmp\[\\t \]*r.,\[\\t \]*#63" 1 } } */
