// PR c++/9537

class String
{
public:
    String();
    String( char *str );
    operator char *();
};

const String operator+( String s1, String )
{
  return s1;
}

String valGlue(const String before)
{
    String ret;
    return false ? ret : before + before;
}
