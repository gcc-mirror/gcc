/* { dg-do compile } */
/* { dg-options "-Wall" } */

typedef int UOW;  /* { dg-error "" } */
struct ABC {
  UOW UOW; /* { dg-error "" } */
};

