// PR inline-asm/85172
// { dg-do compile }
// { dg-options "" }

int
foo ()
{
  return !__builtin_constant_p (({ __asm (""); 0; }));
}
