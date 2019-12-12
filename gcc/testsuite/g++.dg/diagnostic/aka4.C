typedef unsigned int myvec __attribute__((vector_size (16)));

void f (float x)
{
  myvec y = x; // { dg-error {cannot convert 'float' to 'myvec' {aka '__vector\([48]\) unsigned int'} in initialization} }
  myvec *ptr = &x; // { dg-error {cannot convert 'float\*' to 'myvec\*' {aka '__vector\([48]\) unsigned int\*'} in initialization} }
  const myvec *const_ptr = &x; // { dg-error {cannot convert 'float\*' to 'const myvec\*' {aka 'const __vector\([48]\) unsigned int\*'} in initialization} }
  volatile myvec *volatile_ptr = &x; // { dg-error {cannot convert 'float\*' to 'volatile myvec\*' {aka 'volatile __vector\([48]\) unsigned int\*'} in initialization} }
}
