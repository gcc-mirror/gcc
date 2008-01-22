// PR c++/34917
// { dg-do compile }

const int i __attribute ((vector_size (8))) = {};
