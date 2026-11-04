/* Check the errors for invalid arguments to the generic
   __builtin_gcn_simt_broadcast_first_lane.  */

/* { dg-do compile } */

struct st { int a; };
__extension__ typedef __int128 i128_t;

void
f (struct st x, i128_t i128)
{
  __builtin_gcn_simt_broadcast_first_lane (x); /* { dg-error "expects an integer, pointer or floating-point argument" } */
  __builtin_gcn_simt_broadcast_first_lane (i128); /* { dg-error "expects an integer, pointer or floating-point argument" } */
  __builtin_gcn_simt_broadcast_first_lane (); /* { dg-error "expects 1 argument but 0 given" } */
  __builtin_gcn_simt_broadcast_first_lane (1, 2); /* { dg-error "expects 1 argument but 2 given" } */
}
