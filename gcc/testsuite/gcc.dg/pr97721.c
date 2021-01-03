// { dg-do compile }
// { dg-options "-O -fno-tree-dominator-opts" }

int ot;

void
z6 (char *tw)
{ 
  while (ot >= 0)
    --ot;

  __builtin_strcpy (&tw[ot], tw);
}
