// PR c++/85045
// { dg-do compile { target c++11 } }

typedef struct tt {
  unsigned short h;
} tt;

void mainScreen(float a)
{
  tt numlrect = {int(100/a)}; // { dg-error "narrowing conversion" }
}
