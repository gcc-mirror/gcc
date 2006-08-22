typedef struct dw_fde_struct
{
  int decl;
} *dw_fde_ref;
dw_fde_ref fde_table;
unsigned fde_table_in_use;
void output_call_frame_info (void)
{
  unsigned int i;
  dw_fde_ref fde;
  for (i = 0; i < fde_table_in_use; i++)
    {
      fde = &fde_table[i];
      tree_contains_struct_check_failed (fde_table[i].decl);
    }
}
