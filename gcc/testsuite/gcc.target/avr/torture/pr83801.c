/* { dg-options { "-std=gnu99" } } */
/* { dg-do run { target { ! avr_tiny } } } */

__attribute((noipa))
char to_ascii (unsigned i)
{
    static const char __flash code_tab[] = "0123456789";
    return code_tab[i];
}

int main()
{
  if (to_ascii (2) != '2')
    __builtin_abort();

  return 0;
}
