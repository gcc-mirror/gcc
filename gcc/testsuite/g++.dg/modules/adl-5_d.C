// { dg-additional-options -fmodules-ts }

import foo;
import hidden;

int main ()
{
  X x (2);

  if (frob (x) != 2)  // { dg-error "not declared in" }
    return 1;

  // { dg-regexp "\n\[^\n]*adl-5_a.C:8:15: error: 'frob' was not declared in this scope$" }
  if (TPL (x) != 2)  // { dg-message "required from here" }
    return 2;

  return 0;
}
