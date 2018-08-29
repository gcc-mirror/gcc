/* { dg-options "-Wformat" } */

int main (int argc, char **argv)
{
  char buf[10];

  char c[] = "%i";
  unsigned char uc[] = "%i";
  const char cc[] = "%i";
  const unsigned char cuc[] = "%i";

  __builtin_sprintf(buf, (char *)c, 1);
  __builtin_sprintf(buf, (char *)uc, 1);
  __builtin_sprintf(buf, (char *)cc, 1);
  __builtin_sprintf(buf, (char *)cuc, 1); /* { dg-warning "format string is not an array of type 'char'" } */
  __builtin_sprintf(buf, (const char *)L"foo"); /* { dg-warning "format is a wide character string" } */

  return 0;
}
