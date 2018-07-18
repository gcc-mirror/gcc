/* { dg-do compile } */
/* { dg-options "-Wall -pedantic" } */

extern "C" {

typedef int UOW;  /* { dg-message "declared here" } */
struct ABC {
  UOW UOW; /* { dg-error "changes meaning" } */
};

}

