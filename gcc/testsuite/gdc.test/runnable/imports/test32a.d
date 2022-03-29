module imports.test32a;

struct S{
  int i;
}

int f(){
  return S.sizeof; // OK
}
