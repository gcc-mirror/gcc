/* Header file for fold-vec-cmp-char*.c tests.  Used to verify codegen results
   for vec_cmp{eq,ge,gt,le,lt,ne} builtins.  */

#include <altivec.h>

vector bool char
test3_eq (vector signed char x, vector signed char y)
{
  return vec_cmpeq (x, y);
}

vector bool char
test6_eq (vector unsigned char x, vector unsigned char y)
{
  return vec_cmpeq (x, y);
}

vector bool char
test3_ge (vector signed char x, vector signed char y)
{
  return vec_cmpge (x, y);
}

vector bool char
test6_ge (vector unsigned char x, vector unsigned char y)
{
  return vec_cmpge (x, y);
}

vector bool char
test3_gt (vector signed char x, vector signed char y)
{
  return vec_cmpgt (x, y);
}

vector bool char
test6_gt (vector unsigned char x, vector unsigned char y)
{
  return vec_cmpgt (x, y);
}

vector bool char
test3_le (vector signed char x, vector signed char y)
{
  return vec_cmple (x, y);
}

vector bool char
test6_le (vector unsigned char x, vector unsigned char y)
{
  return vec_cmple (x, y);
}

vector bool char
test3_lt (vector signed char x, vector signed char y)
{
  return vec_cmplt (x, y);
}

vector bool char
test6_lt (vector unsigned char x, vector unsigned char y)
{
  return vec_cmplt (x, y);
}

vector bool char
test3_ne (vector signed char x, vector signed char y)
{
  return vec_cmpne (x, y);
}

vector bool char
test6_ne (vector unsigned char x, vector unsigned char y)
{
  return vec_cmpne (x, y);
}

