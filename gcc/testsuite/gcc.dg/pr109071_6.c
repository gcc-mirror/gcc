/* PR tree-optimization/109071 need more context for -Warray-bounds warnings
   due to code duplication from jump threading.
   test case is from PR117179, which is a duplication of PR109071.  */  
/* { dg-options "-O2 -Warray-bounds -fdiagnostics-show-context=1" } */
/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
const char* commands[] = {"a", "b"};

int setval_internal(int comind)
{
  if (comind > sizeof(commands)/sizeof(commands[0])) {
    return 0;
  }

  return 1;
}

_Bool setval_internal_tilde(int comind, const char *com,
			    const char *val)
{
  int ret = setval_internal(comind);
  if (commands[comind] == "b" && /* { dg-warning "is outside array bounds of" } */

      ret)
    return 1;
  return 0;
}
/* { dg-begin-multiline-output "" }
   NN |   if (commands[comind] == "b" &&
      |       ~~~~~~~~^~~~~~~~
  'setval_internal_tilde': events 1-2
   NN |   if (comind > sizeof(commands)/sizeof(commands[0])) {
      |      ^
      |      |
      |      (1) when the condition is evaluated to true
......
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN |   if (commands[comind] == "b" &&
      |       ~~~~~~~~~~~~~~~~
      |               |
      |               (2) warning happens here
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   NN | const char* commands[] = {"a", "b"};
      |             ^~~~~~~~
   { dg-end-multiline-output "" } */
