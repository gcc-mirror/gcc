// { dg-do run  }
// { dg-options "-fshort-enums" }
// Origin: Mark Mitchell <mark@codesourcery.com>

enum E { 
  a = -312
};

E e = a;

int main () {
  if ((int) e != -312)
    return 1;
}
