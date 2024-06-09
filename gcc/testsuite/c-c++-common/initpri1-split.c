/* { dg-do run { target init_priority } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */
/* { dg-additional-sources {initpri1_part_c1.c initpri1_part_c2.c initpri1_part_c3.c initpri1_part_d1.c initpri1_part_d2.c initpri1_part_d3.c initpri1_part_cd4.c initpri1_part_main.c} } */
/* { dg-additional-options -DCDTOR_LINKAGE= } */
