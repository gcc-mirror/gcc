/* This used to ICE because there was no null check in
   check_bitfield_type_and_width. */

struct tree_common
{
  enum tree_code code : 8; /* {dg-error "" "" } */
/* { dg-warning "" "" { target *-*-* } 6 } */
};
