/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-n2" } */

/* Avoid ICE on empty reduction def in info_for_reduction called by
   aarch64_force_single_cycle.
  
   E.g.
     <bb 3> [local count: 858993456]:
     # sum_18 = PHI <sum_15(5), 0(2)>
     sum.0_5 = (unsigned int) sum_18;
     _6 = _4 + sum.0_5;     <-- it is "live" but doesn't have reduction def
     sum_15 = (int) _6;
     ...
     if (ivtmp_29 != 0)
       goto <bb 5>; [75.00%]
     else
       goto <bb 4>; [25.00%]

     <bb 5> [local count: 644245086]:
     goto <bb 3>; [100.00%]

     <bb 4> [local count: 214748368]:
     # _31 = PHI <_6(3)>
     _8 = _31 >> 1;
*/

int
f (unsigned int *tmp)
{
  int sum = 0;
  for (int i = 0; i < 4; i++)
    sum += tmp[i];

  return (unsigned int) sum >> 1;
}
