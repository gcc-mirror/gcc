// { dg-do compile }

typedef int __attribute__((mode(V2SI))) vec;

vec foo[] = { (vec) {1, 2} };
