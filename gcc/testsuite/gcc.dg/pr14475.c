/* This used to ICE because there was no null check in
   check_bitfield_type_and_width. */

struct tree_common
{
  enum tree_code code : 8; /* { dg-error "ISO C forbids forward references to" "forward ref" } */
  /* { dg-error "type of bit-field .code. is a GCC extension" "extension" { target *-*-* } .-1 } */
  /* { dg-warning "narrower than values of its type" "narrower" { target *-*-* } .-2 } */
  /* { dg-error "incomplete type" "incomplete" { target *-*-* } .-3 } */
};
