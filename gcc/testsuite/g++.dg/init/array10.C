// { dg-do compile }
// { dg-options "" }

typedef int __attribute__((vector_size (8))) vec;

vec foo[] = { (vec) {1, 2} };
