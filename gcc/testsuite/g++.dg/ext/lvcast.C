// Copyright (C) 2002 Free Software Foundation
// Contributed by Matt Austern <austern@apple.com>

// { dg-do compile }
// { dg-options -fpermissive }

void f ()
{
  int n;
  (char) n = 1;
}
