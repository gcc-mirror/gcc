// PR C++/18984
//  We just to ICE as we did not add a
// deference to invisible by reference 
// variable

// { dg-do compile }


struct Str
{
    Str(const char *chars);
    Str& operator=(const char *chars);
    virtual operator char*() const;
};
Str _localName(Str fullname)
{
  return (char*)fullname;
}
