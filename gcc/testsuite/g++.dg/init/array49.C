// PR c++/84632
// { dg-additional-options "-w" }

class {
  &a;  // { dg-error "forbids declaration" }
} b[2] = b;  // { dg-error "initialized" }
