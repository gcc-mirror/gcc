// GROUPS passed temps
// Date: Tue, 22 Mar 94 12:46:28 +0100
// From: dak@pool.informatik.rwth-aachen.de
// Message-Id: <9403221146.AA07815@messua>
// Subject: Bad code for pointer to member use as reference in g++ 2.5.8

#include <stdio.h>
struct str {
  int i;
} xxx = {0};

int& test(str *arg1, int str::*arg2)
{
  return (arg1->*arg2);
}

int main()
{
  test(&xxx, &str::i) = 5;
  if (xxx.i == 0)
    printf ("FAIL\n");
  else
    printf ("PASS\n");
}
