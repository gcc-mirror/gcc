/* { dg-do compile } */
/* { dg-require-stack-check "specific" } */
/* { dg-additional-options "-fpermissive -fstack-check -w" } */
/* { dg-require-effective-target alloca } */

int a;
struct b {
    char c;
    void *d;  
};
struct b e() {
    struct b f[] = {{}, "", f, a};
}
