
/* Regression test for paste corner cases.  Distilled from
   syscall stub logic in glibc.  */

/* { dg-do compile } */

#define ENTRY(name)	name##:
#define socket bind

int
main(void)
{
  goto socket;

  ENTRY(socket) /* { dg-error "valid preprocessing token" "" } */
    return 0;
}
