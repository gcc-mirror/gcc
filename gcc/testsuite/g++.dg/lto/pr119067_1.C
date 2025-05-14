/* { dg-options "-mavx2" } */

typedef char __v32qi __attribute__ ((__vector_size__ (32)));
struct ff
{
  __v32qi t;
};
__v32qi g(struct ff a) {
 return a.t;
}
