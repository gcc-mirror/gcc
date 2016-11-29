// { dg-do run }

int
__attribute__((no_sanitize_address))
main (void)
{
  char *ptr;
  char *ptr2;
  {
    char my_char[9];
    ptr = &my_char[0];
    __builtin_memcpy (&ptr2, &ptr, sizeof (ptr2));
  }

  *(ptr2+9) = 'c';
}
