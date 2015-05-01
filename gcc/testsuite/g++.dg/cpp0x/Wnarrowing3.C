// PR c++/65858
// { dg-do compile { target c++11 } }
// { dg-require-effective-target lto }
// { dg-options "-flto -Wno-narrowing" }

int x { 0.5 };
