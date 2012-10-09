/* { dg-do run } */
#define vector __attribute__((vector_size(8*sizeof(short))))

int main (int argc, char *argv[]) {
  vector short v0 = {argc,2,3,4,5,6,7};
  vector short v1 = {2,2,2,2,2,2,2};
  vector short r1,r2,r3,r4;
  int i = 8;

  r1 = v0 << 1;
  r2 = v0 >> 1;

  r3 = v0 << v1;
  r4 = v0 >> v1;

  return 0;
}

