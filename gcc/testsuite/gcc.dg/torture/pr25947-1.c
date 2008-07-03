/* PR target/25947: define_split in cris.md caused unrecognized insn.  */
/* { dg-options "-fpic" { target fpic } } */
/* { dg-skip-if "requires unsupported run-time relocation" { spu-*-* } { "-O0" } { "" } } */

extern char *rl_line_buffer;
extern int rl_point;
extern int rl_end;
static const char *vi_motion = " hl^$0ftFT;,%wbeWBE|";
void
rl_vi_complete (int ignore, int key)
{
  if ((rl_point < rl_end)
      &&
      (!(((rl_line_buffer[rl_point]) == ' ')
	 || ((rl_line_buffer[rl_point]) == '\t'))))
    {
      if (!
	  (((rl_line_buffer[rl_point + 1]) == ' ')
	   || ((rl_line_buffer[rl_point + 1]) == '\t')))
	rl_vi_end_word (1, 'E');
    }
}
