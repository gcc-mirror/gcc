typedef unsigned int myvec __attribute__((vector_size (16)));

void f (float x)
{
  myvec y = x; /* { dg-error {incompatible types when initializing type 'myvec' {aka '__vector\([48]\) unsigned int'} using type 'float'} } */
  myvec *ptr = &x; /* { dg-error {initialization of 'myvec \*' {aka '__vector\([48]\) unsigned int \*'} from incompatible pointer type 'float \*'} } */
  const myvec *const_ptr = &x; /* { dg-error {initialization of 'const myvec \*' {aka 'const __vector\([48]\) unsigned int \*'} from incompatible pointer type 'float \*'} } */
  volatile myvec *volatile_ptr = &x; /* { dg-error {initialization of 'volatile myvec \*' {aka 'volatile __vector\([48]\) unsigned int \*'} from incompatible pointer type 'float \*'} } */
}
