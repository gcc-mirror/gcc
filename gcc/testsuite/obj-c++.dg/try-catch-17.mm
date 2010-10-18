/* Test if addition of 'volatile' to object causes bogus error in presence of try-catch. */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

struct Point {
  short v;
  short h;
};

void foo ()
{
 Point eventLocation;
 @try {
 } @catch (id iiii) {
 }

 Point p = eventLocation;
}
