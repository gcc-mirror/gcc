// PR optimization/13067
// Origin: <bryner@brianryner.com>

// This used to fail on the tree-ssa because of "out-of-ssa"
//  We might have a valid variable, but not a valid value when trying to find
//  useless statements created by out-of-ssa translation. In this case
//  val will be set to null, then later dereferenced.  Bad.

// { dg-do compile }
// { dg-options "-Os" }



struct Iterator
{
  Iterator operator++();
};

void GetChar(char* aChar);

void foo(char aChar)
{
  char quote;
  Iterator end;

  while (1) {
    if (aChar == '"')
      GetChar(&aChar);

    switch (aChar) {
    case 'a':
      ++end;
      if (quote) {
	if (quote == aChar) {
	  quote = 0;
	}
      } else {
	quote = aChar;
      }
    }
  }
}



