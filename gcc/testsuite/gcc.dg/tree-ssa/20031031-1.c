/* { dg-do compile } */
/* { dg-options "-O1" } */

/* This program requires the SSA renamer to be run after the second DOM
   pass.  Test provided by Falk Hueffner as Bugzilla #12825.  */

struct floppy_raw_cmd {
  int flags, track;
} *raw_cmd, default_raw_cmd;

void
setup_format_params (void)
{
  raw_cmd = &default_raw_cmd;
  raw_cmd->track = 0;
  raw_cmd->flags = 0;
}
