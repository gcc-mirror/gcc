// Copyright (C) 2000 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// simplified from libio/floatconv.c

static const double bar[] = { 0 };
int j;

double
foo ()
{
  return bar[j];
}
