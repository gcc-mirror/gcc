/* { dg-do run } */
#define vector __attribute__((vector_size(16) ))

/* Check whether register declaration of vector type still 
   allow us to subscript this type.  */

typedef vector short myvec_t;

struct vec_s {
    vector short member;
};


int main () {
  register short vector v0 = {1,2,3,4,5,6,7};
  register myvec_t v1 = {1,2,3,4,5,6,7};
  register struct vec_s v2;
    
  v2.member = v1;

  short r = v0[0] + v1[1] + v2.member[2];
  if (r != 6)
    __builtin_abort ();

  return 0;
}
