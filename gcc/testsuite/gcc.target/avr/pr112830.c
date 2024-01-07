/* { dg-do compile { target { ! avr_tiny } } } */
/* { dg-options "" } */

typedef __SIZE_TYPE__ size_t;

void copy_n (void *vdst, const __memx void *vsrc, size_t n)
{
  typedef struct { char a[n]; } T;
  T *dst = (T*) vdst;
  const __memx T *src = (const __memx T*) vsrc;
  *dst = *src;
}
