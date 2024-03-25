void long2str (const char *, int);
extern int foo (void);

int
avi_parse_comments (int fd, char *buf, int space_left)
{
  int len = 0, readlen = 0, k;
  char *data, *c, *d;

  if (fd <= 0 || !buf || space_left <= 0)
    return -1;

  __builtin_memset (buf, 0, space_left);

  readlen = foo ();
  if (!(data = __builtin_malloc (readlen * sizeof (char) + 1)))
    return -1;

  c = data;
  space_left--;

  while (len < space_left)
    {
      if (!c || *c == '\0')
	break;
      else if (*c == 'I')
	{
	  d = c + 4;

	  k = 0;
	  while (d[k] != '\r' && d[k] != '\n' && d[k] != '\0')
	    ++k;
	  if (k >= space_left)
	    return len;


	  __builtin_memcpy (buf + len, c, 4);
	  len += 4;


	  long2str (buf + len, k + 1);
	  len += 4;


	  __builtin_memcpy (buf + len, d, k);

	  *(buf + len + k + 1) = '\0';


	  if ((k + 1) & 1)
	    {
	      k++;
	      *(buf + len + k + 1) = '\0';
	    }
	  len += k + 1;


	  while (*c != '\n' && *c != '\0')
	    ++c;
	  if (*c != '\0')
	    ++c;
	  else
	    break;

	}
    }
  __builtin_free (data);

  return len;
}


