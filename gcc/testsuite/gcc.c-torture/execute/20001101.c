/* { dg-require-effective-target untyped_assembly } */
extern void abort(void);
			 
typedef struct
{
  unsigned int unchanging : 1;
} struc, *rtx;

rtx dummy ( int *a, rtx *b)
{
  *a = 1;
  *b = (rtx)7;
  return (rtx)1;
}

void bogus (insn, thread, delay_list)
     rtx insn;
     rtx thread;
     rtx delay_list;
{
  rtx new_thread;
  int must_annul;

  delay_list = dummy ( &must_annul, &new_thread);
  if (delay_list == 0 &&  new_thread )
    {
      thread = new_thread;
    }
  if (delay_list && must_annul)
    insn->unchanging = 1;
  if (new_thread != thread )
    abort();
}

int main()
{
  struc baz;
  bogus (&baz, (rtx)7, 0);
  exit(0);
  return 0;
}
