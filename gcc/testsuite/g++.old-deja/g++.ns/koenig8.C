// Build don't link:
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Dec 1999 <nathan@acm.org>

// caused an ICE determining whether to perform Koenig lookup
// when checking is enabled

template<class T> void Zap (T);

void  V3 ()
{
  Zap (1);
}

