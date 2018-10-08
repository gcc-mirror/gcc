/* { dg-do compile } */
/* { dg-require-effective-target init_priority } */
/* { dg-options "-Wno-prio-ctor-dtor" } */

void construct1 () __attribute__ ((constructor (10)));
void construct2 () __attribute__ ((constructor (100)));
void construct2 () __attribute__ ((constructor (101)));
void destruct1 () __attribute__ ((destructor (1)));
void destruct2 () __attribute__ ((destructor (02)));
void destruct2 () __attribute__ ((destructor (102)));
