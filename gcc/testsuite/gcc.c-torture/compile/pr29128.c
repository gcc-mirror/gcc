/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

typedef unsigned long Eterm;
void
process_main (void)
{
  register Eterm x0;
  register Eterm *reg = ((void *) 0);
  register Eterm *I = ((void *) 0);
  static void *opcodes[] = {
      &&lb_allocate_heap_zero_III,
      &&lb_allocate_init_tIy, &&lb_allocate_zero_tt
  };
lb_allocate_heap_III:{
    Eterm *next;
    goto *(next);
  }
lb_allocate_heap_zero_III:{
  }
lb_allocate_init_tIy:{
  }
lb_allocate_zero_tt:{
    Eterm *next;
    {
      Eterm *tmp_ptr = ((Eterm *) (((x0)) - 0x1));
      (*(Eterm *) (((unsigned char *) reg) + (I[(0) + 1]))) = ((tmp_ptr)[0]);
      x0 = ((tmp_ptr)[1]);
    }
    goto *(next);
  }
}
