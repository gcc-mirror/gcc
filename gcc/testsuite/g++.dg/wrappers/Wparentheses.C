// { dg-options "-Wparentheses" }

extern char read_skip_spaces ();

void test ()
{
  char c;
  while ((c = read_skip_spaces ()) && c != ']')
    ;
}
