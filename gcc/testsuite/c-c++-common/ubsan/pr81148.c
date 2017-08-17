/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

int x = -106;
int main()
{
  // -123 - (0x8000000000000000 - -1)
  return (-123 - ((9223372036854775806LL ^ ~(x && 1)) - -1)) == 0;
}
