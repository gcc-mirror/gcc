#include <stream.h>
#include <strstream.h>

int
main(int, char* [])
{
  strstream s;

  s << "line 1\nline 2\n\nline 4";
  s << ends;

  int nLine = 0;

  while( true ) {
    char* line = 0;
    s.gets(&line);

    if( ! line ) {
      break;
    }

    ++nLine;
    cout << nLine << ": " << line << endl;

    if( nLine > 10 ) {  // stop infinite loop
      break;
    }
  }
  return 0;
}
