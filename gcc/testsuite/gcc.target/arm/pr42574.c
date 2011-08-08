/* { dg-options "-mthumb -Os -fpic" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-require-effective-target fpic } */
/* Make sure the address of glob.c is calculated only once and using
   a logical shift for the offset (200<<1).  */
/* { dg-final { scan-assembler-times "lsl" 1 } } */

struct A {
 char a[400];
 float* c;
};
struct A glob;
void func();
void func1(float*);
int func2(float*, int*);
void func3(float*);

void test(int *p) {
 func1(glob.c);
 if (func2(glob.c, p)) {
   func();
 }
 func3(glob.c);
}
