/* Copyright (C) 2000  Free Software Foundation.
   by William Cohen  <wcohen@redhat.com>  */

/* { dg-do compile } */
/* { dg-options "" } */

struct PDATA
{
    unsigned int  Dummy:32;
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
    { 1, "PName0" },
  } /* { dg-warning "(deprecated initialization)|(near initialization)" "" } */
};
