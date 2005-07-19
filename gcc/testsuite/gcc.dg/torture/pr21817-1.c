/* hppa*-*-hpux* needs -fno-common so that value can be given a 16
   byte alignment.  */

/* { dg-do compile } */
/* { dg-options "-fno-common" { target { hppa*-*-hpux* } } } */

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
