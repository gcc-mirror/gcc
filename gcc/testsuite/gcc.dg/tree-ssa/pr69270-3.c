/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-uncprop-details -w" } */

/* We're looking for a constant argument a PHI node.  There
   should only be one if we unpropagate correctly.  */
/* { dg-final { scan-tree-dump-times ", 1" 4 "uncprop1"} } */

typedef long unsigned int size_t;
typedef union gimple_statement_d *gimple;
unsigned char
propagate_with_phi ()
{
  gimple use_stmt;
  unsigned char phi_inserted;
  phi_inserted = 0;
  for (; !end_imm_use_stmt_p (); next_imm_use_stmt ())
    {
      if (!(arf () == 10 && boo () == 20))
        continue;
      if (!phi_inserted)
        phi_inserted = 1;
      else
        update_stmt ();
    }
}

