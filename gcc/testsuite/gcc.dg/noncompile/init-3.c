void die(void)
{
  char foo[BAR] = "1234"; /* { dg-error "undeclared|function|incomplete type" } */
}
