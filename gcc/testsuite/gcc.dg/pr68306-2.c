/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mno-sse -mno-mmx" { target i?86-*-* x86_64-*-* } } */

struct {
    int tz_minuteswest;
    int tz_dsttime;
} a, b;
void fn1() {
    b.tz_minuteswest = a.tz_minuteswest;
    b.tz_dsttime = a.tz_dsttime;
}
