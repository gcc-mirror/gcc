// { dg-do run  }
// { dg-options "-fconst-strings" }
// 981203 bkoz
// g++/15351 + test

#include <assert.h>

bool gtest;

struct acapulco {
  acapulco(const char *) { gtest = true; }
  acapulco(char *) { gtest = false; }
};

void foo(void)
{
  acapulco("some such string\n");
}

int main() 
{
  foo();
  if (!gtest)
    assert (0);

  return !gtest;
}

