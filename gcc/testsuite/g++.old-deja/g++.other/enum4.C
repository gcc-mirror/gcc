// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -fshort-enums

enum E { 
  a = -312
};

E e = a;

int main () {
  if ((int) e != -312)
    return 1;
}
