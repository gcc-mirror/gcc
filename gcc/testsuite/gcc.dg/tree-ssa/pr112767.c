/* { dg-do compile } */
/* { dg-options "-O3" } */

double reg_dict[32];

void foo(int);

void initialize()
{
  int i=8;
  for (int phi=0; phi<8; ++phi) {
    reg_dict[i]=0; /* { dg-bogus "undefined behavior" } */
    int sn = 0;
    if (i < 16) sn = 20;
    foo(sn);
    ++i;
  }
}
