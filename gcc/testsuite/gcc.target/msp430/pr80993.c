/* { dg-do link } */
/* { dg-options "--save-temps -msim -flto -Os" } */
/* { dg-final { scan-file "pr80993.exe.ltrans0.s" no_ref_handler } } */

void __attribute__((interrupt)) no_ref_handler (void)
{
  while (1);
}

int main (void)
{
  return 0;
}
