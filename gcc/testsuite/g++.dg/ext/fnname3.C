// Test whether __func__ works for constructors and destructors.

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Matt Austern <austern@apple.com>, 3 Aug 2003
// { dg-do run }

struct uk9i
{
  uk9i();
  ~uk9i();

  static const char* fname;
  static bool obj_exists;
};

uk9i::uk9i()
{
  obj_exists = true;
  fname = __func__;
}

uk9i::~uk9i()
{
  obj_exists = false;
  fname = __func__;
}

const char* uk9i::fname = 0;
bool uk9i::obj_exists = false;

int main()
{
  bool ok = true;

  ok = ok && uk9i::fname == 0;
  ok = ok && !uk9i::obj_exists;

  {
    uk9i tmp;
    ok = ok && uk9i::obj_exists;
    ok = ok && uk9i::fname != 0;
    if (ok)
      {
	ok = ok && uk9i::fname[0] == 'u';
	ok = ok && uk9i::fname[1] == 'k';
	ok = ok && uk9i::fname[2] == '9';
	ok = ok && uk9i::fname[3] == 'i';
	ok = ok && uk9i::fname[4] == '\0';
      }
  }

  ok = ok && !uk9i::obj_exists;
  ok = ok && uk9i::fname != 0;
  if (ok)
    {
      ok = ok && uk9i::fname[0] == '~';
      ok = ok && uk9i::fname[1] == 'u';
      ok = ok && uk9i::fname[2] == 'k';
      ok = ok && uk9i::fname[3] == '9';
      ok = ok && uk9i::fname[4] == 'i';
      ok = ok && uk9i::fname[5] == '\0';
    }  

  return ok ? 0 : 1;
}
