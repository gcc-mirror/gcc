/* { dg-do compile } 
   { dg-options "-O2 -finstrument-functions-once" } */

void func(int n)
{
        struct T { int x[n]; };
	struct T *t = __builtin_malloc(sizeof *t);
}

