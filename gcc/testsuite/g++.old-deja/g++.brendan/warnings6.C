// Build don't link: 
// Special g++ Options: -O2 -Wall -ansi -pedantic
// GROUPS passed warnings
// should not emit this warning about func:
// 	x.C:2: warning: invalid storage class for function `func'
//
template <class T> inline void func(T)
{
}
