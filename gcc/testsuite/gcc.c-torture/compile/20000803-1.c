static int      gl_cnt = 0;
static char     gl_buf[1024];

void
gl_yank()
{
  int  i;

  for (i=gl_cnt; i >= 0; i--)
    gl_buf[i+10] = gl_buf[i];
}
