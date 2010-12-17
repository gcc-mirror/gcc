/* Test if addition of 'volatile' to object causes bogus error in presence of try-catch. */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

extern void func( void * outData) ;
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

   func( &eventLocation );
}
