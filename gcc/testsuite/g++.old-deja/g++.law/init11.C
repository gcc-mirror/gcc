// GROUPS passed initialization
// init file
// From: hansen@srd.ull.rdc.toshiba.co.jp
// Date:     Mon, 13 Dec 93 18:27:51 +0900
// Subject:  g++ Bug
// Message-ID: <9312130927.AA08192@VLCS151.noname>

#include <stdio.h>

int X = 7;

struct foo {
   int a,b,c;
};

struct foo Ack = {5, X, 3};

int main()
{
  if (Ack.a == 5 && Ack.b == 7 && Ack.c == 3)
    printf ("PASS\n");
  else
    { printf ("FAIL\n"); return 1; }
}
