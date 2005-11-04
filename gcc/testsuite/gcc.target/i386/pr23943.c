/* This used to ICE in side_effects_p, due to a problem in cse.c.
   Origin: marcus at jet dot franken dot de.  */
/* { dg-do compile { target x86_64-*-* } } */
/* { dg-options "-O2 -fPIC" } */

typedef long unsigned int size_t;

extern size_t strlen (__const char *__s)
    __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));

static char savecallsin[256] = "";

int read_agent_config(void)
{
  savecallsin[0] = '\0';

  if (savecallsin[strlen(savecallsin) - 1] != '/')
    __builtin___strncat_chk (savecallsin, "/", sizeof(savecallsin) - strlen(savecallsin) - 1, __builtin_object_size (savecallsin, 2 > 1)) ;
  return 0;
}
