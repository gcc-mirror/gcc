/* PR c++/590 */
/* { dg-do compile } */

#include <iostream>
#include <sstream>
using namespace std;

enum ExternalEnum { EXTERNAL_VALUE = 2 };

class BadStream : public ostringstream 
{    
public: 
  enum InternalEnum { VALUE = 0 };   
  BadStream( InternalEnum e ) {};
  BadStream( InternalEnum e, int i ) {};
};

int main() 
{
  ( BadStream( BadStream::VALUE ) ) << "foobar" << endl;

  BadStream((BadStream::InternalEnum)BadStream::VALUE ) << "foobar";

  BadStream::InternalEnum in = BadStream::VALUE;
  BadStream( in ) << "foobar";
    
  BadStream( BadStream::VALUE, 0 ) << "foobar" << endl;
    
  // This didn't used to compile:
  BadStream( BadStream::VALUE ) << "foobar" << endl;

  return 0;
}
