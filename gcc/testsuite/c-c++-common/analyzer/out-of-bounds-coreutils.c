/* Reduced from coreutils/ls.c attach.  */

void add_zero_terminator (char *buf)
{
  char *end = buf;
  while (end++); /* TODO: arguably we should report this.  */
  if (buf < end)
    end[-1] = '\0';
}

/* Reduced from coreutils/cat.c.  */

#define LINE_COUNTER_BUF_LEN 20
static char line_buf[LINE_COUNTER_BUF_LEN] =
  {
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '0',
    '\t', '\0'
  };

/* Position of the first digit in 'line_buf'.  */
static char *line_num_start = line_buf + LINE_COUNTER_BUF_LEN - 3;

static void
next_line_num (void)
{
  if (line_num_start > line_buf)
    *--line_num_start = '1';
}
