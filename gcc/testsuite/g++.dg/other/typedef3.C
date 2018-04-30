// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/40357
// { dg-do compile }

struct XalanCProcessor
{
      typedef enum {eInvalid, eXalanSourceTree, eXercesDOM} ParseOptionType;
          ParseOptionType getParseOption(void);
};
typedef XalanCProcessor::ParseOptionType ParseOptionType;
ParseOptionType XalanCProcessor::getParseOption(void) { return ParseOptionType(); }

