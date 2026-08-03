// PR target/126484
// { dg-do compile }

int
foo ()
{
  return __builtin_mips_get_fcsr ();
}
