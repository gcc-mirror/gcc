// PR c++/91024
// { dg-do compile { target c++11 } }
// { dg-options "-Wimplicit-fallthrough" }

int
foo (char c)
{
  int result = 0;

  switch (c)
    {
    case 'O':
    case 'K':
      return result;
    [[unlikely]] case 'X':	// { dg-bogus "this statement may fall through" }
    case 'x':			// { dg-bogus "here" }
      return result;
    default:
      break;
    }
  return result;
}
