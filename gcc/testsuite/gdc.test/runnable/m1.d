/*
EXTRA_SOURCES: imports/m1a.d
PERMUTE_ARGS:
RUN_OUTPUT:
---
Success
---
*/

module m1;

import core.stdc.stdio;

import imports.m1a;

class A
{
}

A createA() { return new A; }

alias A function() aliasM1;

void main()
{
  aFunc( &createA );
  printf("Success\n");
}
