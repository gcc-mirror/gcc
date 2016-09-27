// PR c++/77722
// { dg-do run }
// { dg-options "-fsanitize=return -w" }
// { dg-shouldfail "ubsan" }

int
foo ()
{
}

int
main ()
{
  foo ();
  return 0;
}

// { dg-output "execution reached the end of a value-returning function without returning a value" }
