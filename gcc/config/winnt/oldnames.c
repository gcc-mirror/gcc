int
access (const char *path, int mode)
{
  return _access (path, mode);
}

int
chmod (const char *filename, int pmode)
{
  return _chmod (filename, pmode);
}

int
close (int handle)
{
  return _close (handle);
}

char *
mktemp (char *template)
{
   return (char *) _mktemp (template);
}

int
open (const char *filename, int oflag, int pmode)
{
  return _open (filename, oflag, pmode);
} 

int
read (int handle, void *buffer, unsigned int count)
{
  return _read (handle, buffer, count);
}

int
unlink (const char *path)
{
  return _unlink (path);
}

int
write (int handle, void *buffer, unsigned int count)
{
  return _write (handle, buffer, count);
}
