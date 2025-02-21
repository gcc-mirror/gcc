/* { dg-do compile } */
/* { dg-require-stack-check "specific" } */
/* { dg-additional-options "-fpermissive -fstack-check -w" } */

int a;
struct b {
    char c;
    void *d;  
};
struct b e() {
    struct b f[] = {{}, "", f, a};
}
