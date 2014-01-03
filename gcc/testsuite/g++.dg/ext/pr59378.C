// { dg-do compile }
typedef int v4si __attribute__ ((vector_size (4*sizeof(int))));
template<int C>
void traverse(v4si& bounds){
  v4si m = {0,1,2,3};
  bounds = __builtin_shuffle(bounds, m);
}
template void traverse<0>(v4si&);
