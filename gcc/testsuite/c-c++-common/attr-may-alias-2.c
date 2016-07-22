/* We used to reject this because types differentiating only in
   TYPE_REF_CAN_ALIAS_ALL were deemed incompatible.  */
/* { dg-do compile } */

struct sockaddr;
struct sockaddr *f (void);

struct __attribute__((may_alias)) sockaddr { int j; };
struct sockaddr *
f (void)
{
  return
#ifndef __cplusplus
    (void *)
#endif
    0;
}
