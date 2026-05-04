/* { dg-do compile } */

#include <stdint.h>

int64_t g1, g8;

void g14()
{
  int16_t v5;
  goto lbl_cont30;
lbl_bf2:
  g8 = 0;
lbl_b5:
  g1 = g1 - 8366249724514421075;
  switch (v5)
case 5:
case 2:
    goto lbl_b5;
    goto lbl_bf2;
lbl_cont30:
    switch (v5)
      {
      case 0: goto lbl_bf2;
      case 5: goto lbl_b5;
      }
}
