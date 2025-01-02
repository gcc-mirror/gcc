/* { dg-do compile } */
/* { dg-options "-std=c23" } */

struct s1 {
  int f1;
};
struct s2 {
  int f2;
};
struct s1 f(struct s2 *);

struct s1 {
  int f1;
};
struct s2 {
  int f2;
};
struct s1 f(struct s2 *);
