/* Copyright (C) 2000  Free Software Foundation.
   by William Cohen  <wcohen@redhat.com>  */

/* { dg-do compile } */
/* { dg-options "" } */
#include <limits.h>

struct PDATA
{
    unsigned int  Dummy:(sizeof(int)*CHAR_BIT);
    const char*   PName;
};

typedef struct PDATA    P_DATA;

struct PLAYBOOK {
        const char * BookName;
        P_DATA       Play[0];
};

struct PLAYBOOK playbook  =
{
  "BookName",
  {
    { 1, "PName0" }, /* { dg-warning "(excess elements)|(near initialization)" } */
  }
};
