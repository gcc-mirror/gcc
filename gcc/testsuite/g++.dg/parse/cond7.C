// PR c++/84588

bool (foo()) { return 0; } // declaration

void bar()
{
  if (bool (foo())); // expression

  for (;bool (foo());); // expression

  while (bool (foo())); // expression
}
