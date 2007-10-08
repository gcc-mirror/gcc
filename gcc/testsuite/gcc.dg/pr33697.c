/* { dg-do compile } */

/* We used to ICE for this with type-checking enabled.  */

typedef signed short gint16;
typedef unsigned short guint16;
gint16 dissect_old_pflog(gint16 rnr)
{
  return (guint16) ((guint16) ((guint16)rnr >> 8) | (guint16) ((guint16)rnr << 8));
}
