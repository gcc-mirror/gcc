// { dg-do assemble  }
// g++ 1.37.1 bug 900404_02

// g++ fails to treat multicharacter literals as type "int" as required by
// section 2.5.2 of the C++ Reference Manual.

// The result is that the following program will exit with a nonzero
// exit status.

// keywords: character literals, multi-character literals, int type

int exit_status = 0;

void function0 (int i)		// function that should be called
{
  i = i;
}

void function0 (char c)		// function that is actually called
{
  c = c;
  exit_status++;
}

int main () { function0 ('abcd'); return exit_status; }		// { dg-warning "" } 
