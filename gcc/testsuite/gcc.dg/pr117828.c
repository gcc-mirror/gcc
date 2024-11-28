/* { dg-do compile } */
/* { dg-options "-g" } */

struct {
  struct {
    int Reserved : 32;
  } u;
} v;
struct {
  struct {
    int Reserved;
  } u;
} w;

