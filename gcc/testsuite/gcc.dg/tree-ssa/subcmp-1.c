/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

_Bool subgtlt_lt(int a00, int b00)
{
  _Bool x00 = a00 > b00;
  _Bool y00 = a00 < b00;
  return x00 - y00 < 0;
}
/* { dg-final { scan-tree-dump "a00_\[0-9\]+.D. < b00_\[0-9\]+.D.|b00_\[0-9\]+.D. > a00_\[0-9\]+.D." "optimized" } } */

_Bool subgtlt_le(int a01, int b01)
{
  _Bool x01 = a01 > b01;
  _Bool y01 = a01 < b01;
  return x01 - y01 <= 0;
}
/* { dg-final { scan-tree-dump "a01_\[0-9\]+.D. <= b01_\[0-9\]+.D.|b01_\[0-9\]+.D. >= a01_\[0-9\]+.D." "optimized" } } */

_Bool subgtlt_gt(int a02, int b02)
{
  _Bool x02 = a02 > b02;
  _Bool y02 = a02 < b02;
  return x02 - y02 > 0;
}
/* { dg-final { scan-tree-dump "a02_\[0-9\]+.D. > b02_\[0-9\]+.D.|b02_\[0-9\]+.D. < a02_\[0-9\]+.D." "optimized" } } */

_Bool subgtlt_ge(int a03, int b03)
{
  _Bool x03 = a03 > b03;
  _Bool y03 = a03 < b03;
  return x03 - y03 >= 0;
}
/* { dg-final { scan-tree-dump "a03_\[0-9\]+.D. >= b03_\[0-9\]+.D.|b03_\[0-9\]+.D. <= a03_\[0-9\]+.D." "optimized" } } */

_Bool subgele_lt(int a04, int b04)
{
  _Bool x04 = a04 >= b04;
  _Bool y04 = a04 <= b04;
  return x04 - y04 < 0;
}
/* { dg-final { scan-tree-dump "a04_\[0-9\]+.D. < b04_\[0-9\]+.D.|b04_\[0-9\]+.D. > a04_\[0-9\]+.D." "optimized" } } */

_Bool subgele_le(int a05, int b05)
{
  _Bool x05 = a05 >= b05;
  _Bool y05 = a05 <= b05;
  return x05 - y05 <= 0;
}
/* { dg-final { scan-tree-dump "a05_\[0-9\]+.D. <= b05_\[0-9\]+.D.|b05_\[0-9\]+.D. >= a05_\[0-9\]+.D." "optimized" } } */

_Bool subgele_gt(int a06, int b06)
{
  _Bool x06 = a06 >= b06;
  _Bool y06 = a06 <= b06;
  return x06 - y06 > 0;
}
/* { dg-final { scan-tree-dump "a06_\[0-9\]+.D. > b06_\[0-9\]+.D.|b06_\[0-9\]+.D. < a06_\[0-9\]+.D." "optimized" } } */

_Bool subgele_ge(int a07, int b07)
{
  _Bool x07 = a07 >= b07;
  _Bool y07 = a07 <= b07;
  return x07 - y07 >= 0;
}
/* { dg-final { scan-tree-dump "a07_\[0-9\]+.D. >= b07_\[0-9\]+.D.|b07_\[0-9\]+.D. <= a07_\[0-9\]+.D." "optimized" } } */

_Bool subltgt_lt(int a08, int b08)
{
  _Bool x08 = a08 < b08;
  _Bool y08 = a08 > b08;
  return x08 - y08 < 0;
}
/* { dg-final { scan-tree-dump "a08_\[0-9\]+.D. > b08_\[0-9\]+.D.|b08_\[0-9\]+.D. < a08_\[0-9\]+.D." "optimized" } } */

_Bool subltgt_le(int a09, int b09)
{
  _Bool x09 = a09 < b09;
  _Bool y09 = a09 > b09;
  return x09 - y09 <= 0;
}
/* { dg-final { scan-tree-dump "a09_\[0-9\]+.D. >= b09_\[0-9\]+.D.|b09_\[0-9\]+.D. <= a09_\[0-9\]+.D." "optimized" } } */

_Bool subltgt_gt(int a10, int b10)
{
  _Bool x10 = a10 < b10;
  _Bool y10 = a10 > b10;
  return x10 - y10 > 0;
}
/* { dg-final { scan-tree-dump "a10_\[0-9\]+.D. < b10_\[0-9\]+.D.|b10_\[0-9\]+.D. > a10_\[0-9\]+.D." "optimized" } } */

_Bool subltgt_ge(int a11, int b11)
{
  _Bool x11 = a11 < b11;
  _Bool y11 = a11 > b11;
  return x11 - y11 >= 0;
}
/* { dg-final { scan-tree-dump "a11_\[0-9\]+.D. <= b11_\[0-9\]+.D.|b11_\[0-9\]+.D. >= a11_\[0-9\]+.D." "optimized" } } */

_Bool sublege_lt(int a12, int b12)
{
  _Bool x12 = a12 <= b12;
  _Bool y12 = a12 >= b12;
  return x12 - y12 < 0;
}
/* { dg-final { scan-tree-dump "a12_\[0-9\]+.D. > b12_\[0-9\]+.D.|b12_\[0-9\]+.D. < a12_\[0-9\]+.D." "optimized" } } */

_Bool sublege_le(int a13, int b13)
{
  _Bool x13 = a13 <= b13;
  _Bool y13 = a13 >= b13;
  return x13 - y13 <= 0;
}
/* { dg-final { scan-tree-dump "a13_\[0-9\]+.D. >= b13_\[0-9\]+.D.|b13_\[0-9\]+.D. <= a13_\[0-9\]+.D." "optimized" } } */

_Bool sublege_gt(int a14, int b14)
{
  _Bool x14 = a14 <= b14;
  _Bool y14 = a14 >= b14;
  return x14 - y14 > 0;
}
/* { dg-final { scan-tree-dump "a14_\[0-9\]+.D. < b14_\[0-9\]+.D.|b14_\[0-9\]+.D. > a14_\[0-9\]+.D." "optimized" } } */

_Bool sublege_ge(int a15, int b15)
{
  _Bool x15 = a15 <= b15;
  _Bool y15 = a15 >= b15;
  return x15 - y15 >= 0;
}
/* { dg-final { scan-tree-dump "a15_\[0-9\]+.D. <= b15_\[0-9\]+.D.|b15_\[0-9\]+.D. >= a15_\[0-9\]+.D." "optimized" } } */
/* { dg-final { scan-tree-dump-not "_\[0-9\]+ - _\[0-9\]+" "optimized" } } */
