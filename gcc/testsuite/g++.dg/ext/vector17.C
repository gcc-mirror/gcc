/* { dg-do compile } */
typedef double __attribute__((vector_size(1024) )) vec;

template <class T>
void f (T *p)
{
  p->~T();
}
void g (vec *p)
{
  f(p);
}
