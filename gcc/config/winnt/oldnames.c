int open (const char *filename, int oflag, int pmode)
{
  return _open (filename, oflag, pmode);
} 

int read (int handle, void *buffer, unsigned int count)
{
  return _read (handle, buffer, count);
}

int close (int handle)
{
  return _close (handle);
}

int access (const char *path, int mode)
{
  return _access (path, mode);
}

char *mktemp (char *template)
{
   return (char *) _mktemp (template);
}
