// PR c++/85618
// { dg-additional-options "-Wno-vla" }

  void function(int size) {
     bool myArray[size][size] = {};
  }
