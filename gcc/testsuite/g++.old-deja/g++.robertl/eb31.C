// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
#include<iostream>

int main() {
  try {
    throw 1;
  } catch(...) {
   try {
     throw;
   } catch(int) {
   }
   try {
     throw;
   } catch(int) {
   }
  }
  return 0;
}


