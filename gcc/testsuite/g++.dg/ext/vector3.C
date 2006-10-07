/* { dg-do compile } */
/* { dg-options "" } */

// PR c++/28302

int __attribute__((vector_size(8))) x;

void foo()
{
  ~x;
}

