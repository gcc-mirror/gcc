// { dg-do run }
// { dg-options "-fabi-version=0" }

struct E1 {};
struct E2 : public E1 {};
struct S1 { int i; };
struct S2 : public S1, E2 {};
#if __cpp_attributes
struct S22 : public S1
{
  [[no_unique_address]] E2 e2;
} s22;
#endif

S2 s2;

int main () {
  if ((char *)(E2*) &s2 != (char *)&s2)
    return 1;
#if __cpp_attributes
  if ((char *)&s22.e2 != (char *)&s22)
    return 2;
#endif
}
