typedef unsigned char __attribute__((__vector_size__ (8))) V;

void
foo (V *v)
{
  *v =  (V) 0x107B9A7FF >= (*v <= 0);
}
