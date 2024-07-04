/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2 -mexplicit-relocs -mcmodel=normal -fdump-rtl-expand -fno-stack-protector" } */
/* { dg-final { scan-rtl-dump-times "mem/u" 2 "expand" } } */

#include <bits/stdc++.h>

using namespace std;

int lr[100005][2];

void
test(void)
{
  int n;

  cin >> n;
  for (int i = 0; i < n; ++i)
    cin >> lr[i][0] >> lr[i][1];
}
