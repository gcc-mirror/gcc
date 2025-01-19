/* PR c/114014
 * { dg-do compile }
 * { dg-options "-std=gnu23 -g" } */

struct r {
  int a;
  char b[];
};
struct r {
  int a;
  char b[0];
};


