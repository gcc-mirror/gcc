/* { dg-do compile } */
/* { dg-options "-g -std=gnu23" } */

enum fmt_type;

void foo(const enum fmt_type a);

enum [[gnu::packed]] fmt_type {
  A
} const a;

