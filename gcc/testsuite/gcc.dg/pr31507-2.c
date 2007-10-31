/* PR target/31507 */
/* { dg-do compile } */
/* { dg-options "-Os -fno-omit-frame-pointer" } */

typedef int (*closure_test_type3)(float, float, float, float, float, float,
				  float, float, double, int, float, float, int,
				  float, float, int);
int f (closure_test_type3 pcl)
{
  int res;
  res = (pcl)
    (1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9, 10, 11.11, 12.0, 13,
     19.19, 21.21, 1);
}
