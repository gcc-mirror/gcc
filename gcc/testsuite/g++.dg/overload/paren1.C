// PR c++/104618

extern void gen_addsi3 (void);
void output_stack_adjust ()
{
  (*(gen_addsi3)) ();
}
