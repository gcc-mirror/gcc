// { dg-do run } 
// { dg-options "-w -fabi-version=0" }

struct S {
  char c : 1024;
};

S s;

int main () {
  s.c = 1;
  if (*(char *)&s != 1)
    return 1;
}
