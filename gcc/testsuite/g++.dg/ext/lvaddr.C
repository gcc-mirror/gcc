// Copyright (C) 2002 Free Software Foundation
// Contributed by Matt Austern <austern@apple.com>

// { dg-do compile }

void f()
{
  int n;
  char* p = &(char) n;		// { dg-error "14:lvalue" }
}
