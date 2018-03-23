// PR tree-optimization/84468 - bogus -Wstringop-truncation despite
// assignment after conditional strncpy
// Compile with -g to verify the warning deals properly with debug
// statements.
// { dg-do compile }
// { dg-options "-O2 -Wstringop-truncation -g" }

extern "C" char* strncpy (char*, const char*, __SIZE_TYPE__);

char d[3];

void g ();

void fnowarn1 (const char *s)
{
  // Update dummy but never actually use it so it's eliminated
  // but causes debugging statements to be emitted for each
  // modification.
  int dummy = 0;

  try
    {
      g ();
      strncpy (d, s, sizeof d);   // { dg-bogus "\\\[-Wstringop-truncation]" }
      ++dummy;
    }
  catch (...)
    {
      ++dummy;
      d[0] = 0;
    }

  ++dummy;
  d[sizeof d - 1] = 0;
}

void fnowarn2 (const char *s)
{
  int dummy = 0;

  try
    {
      g ();
      strncpy (d, s, sizeof d);
      ++dummy;
    }
  catch (...)
    {
      ++dummy;
      return;
    }

  ++dummy;
  d[sizeof d - 1] = 0;
}

void fnowarn3 (const char *s)
{
  int dummy = 0;

  try
    {
      g ();
      strncpy (d, s, sizeof d);
      ++dummy;
      try
	{
	  ++dummy;
	  d[sizeof d - 1] = 0;
	  g ();
	}
      catch (...)
	{
	  ++dummy;
	}
    }
  catch (...)
    {
      ++dummy;
      return;
    }

  ++dummy;
  d[sizeof d - 1] = 0;
}

void fnowarn4 (const char *s)
{
  int dummy = 0;

  try
    {
      g ();
    }
  catch (...)
    {
      strncpy (d, s, sizeof d);   // { dg-bogus "\\\[-Wstringop-truncation]" "bug 84468" { xfail *-*-*} }
      ++dummy;
    }

  ++dummy;
  d[sizeof d - 1] = 0;
}

void fwarn1 (const char *s)
{
  int dummy = 0;

  try
    {
      ++dummy;
      g ();
      ++dummy;
      strncpy (d, s, sizeof d);   // { dg-warning "\\\[-Wstringop-truncation]" }
      ++dummy;
    }
  catch (...)
    {
      ++dummy;
    }

  ++dummy;
}

void fwarn2 (const char *s)
{
  int dummy = 0;

  try
    {
      ++dummy;
      strncpy (d, s, sizeof d);   // { dg-warning "\\\[-Wstringop-truncation]" }
      ++dummy;
      g ();
      ++dummy;
    }
  catch (...)
    {
      ++dummy;
    }

  ++dummy;
}

void fwarn3 (const char *s)
{
  int dummy = 0;

  try
    {
      ++dummy;
      g ();
      ++dummy;
      strncpy (d, s, sizeof d);   // { dg-warning "\\\[-Wstringop-truncation]" }
      ++dummy;
    }
  catch (...)
    {
      ++dummy;
      d[0] = 0;
    }

  ++dummy;
}
