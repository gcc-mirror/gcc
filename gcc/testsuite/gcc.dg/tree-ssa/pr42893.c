/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

struct frame_info;
void tui_registers_changed_hook (void);
extern struct frame_info *deprecated_selected_frame;
int tui_refreshing_registers = 0;
void
tui_registers_changed_hook (void)
{
  struct frame_info *fi;
  fi = deprecated_selected_frame;
  if (fi)

    {
      if (tui_refreshing_registers == 0)

	{
	  tui_refreshing_registers = 1;
	  tui_refreshing_registers = 0;
	}
    }
  return;
}

/* No references to tui_refreshing_registers should remain.  */
/* { dg-final { scan-tree-dump-not "tui_refreshing_registers" "optimized"} } */


