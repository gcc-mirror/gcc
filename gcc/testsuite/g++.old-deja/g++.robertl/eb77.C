#include <iostream>
#include <sstream>

using namespace std;

int
main(int, char* [])
{
  stringstream s;

  s << "line 1\nline 2\n\nline 4";
  s << std::ends;

  int nLine = 0;

  while( true ) {
    char line[100];
    s.get(line, 100);

    if( ! line ) {
      break;
    }

    ++nLine;
    std::cout << nLine << ": " << line << std::endl;

    if( nLine > 10 ) {  // stop infinite loop
      break;
    }
  }
  return 0;
}
