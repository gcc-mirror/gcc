// PR c++/21228
/* { dg-options "-Wunreachable-code" } */

class testStringBase
{
public:
  char *stringPtr;
};

class testString : public testStringBase
{
public:
  testString();
};

testString::testString()
{
  stringPtr = (char *) 9;
}
 
int main(int argc, char **argv) {
  testString s;
}
