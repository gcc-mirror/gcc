/* { dg-do compile } */

typedef float v4sf __attribute__((vector_size(16)));
v4sf value;
void foo(void)
{
  unsigned int band;
  for(band=0; band < 2; band++)
    {
      value += (v4sf){1e9f,1e9f,1e9f,1e9f};
    }
}
