// { dg-do run  }
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


