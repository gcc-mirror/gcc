/* { dg-do compile } */
/* { dg-additional-options "-O -fno-tree-sink -ftree-vectorize" } */
int buffer_ctrl_ctx_0, buffer_ctrl_p1, buffer_ctrl_cmd;

int
buffer_ctrl (long ret, int i)
{
  switch (buffer_ctrl_cmd)
    {
    case 1:
      buffer_ctrl_ctx_0 = 0;
      for (; i; i++)
	if (buffer_ctrl_p1)
	  ret++;
    }
  return ret;
}
