/* { dg-do compile } */
/* { dg-options "-Wall -pedantic" } */

extern "C" {

typedef int UOW;  /* { dg-error "" } */
struct ABC {
  UOW UOW; /* { dg-error "" } */
};

}

