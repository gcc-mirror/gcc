/* { dg-do compile } */

struct A
{
  char c[1];
} a;

const __SIZE_TYPE__ i = (__SIZE_TYPE__)&a.c[0] - 1;
