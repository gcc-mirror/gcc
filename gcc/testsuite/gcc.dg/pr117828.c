/* { dg-do compile } */
/* { dg-options "-g" } */
/* { dg-require-effective-target int32plus } */

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

