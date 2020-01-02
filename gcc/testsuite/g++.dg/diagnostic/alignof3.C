struct A { long i: 2; };
void f()
{
  __alignof(A::i); // { dg-error "16:invalid application of .__alignof. to a bit-field" }
}
