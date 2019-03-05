// { dg-do compile { target c++11 } }

// Reduced from Mozilla SpiderMonkey, licensed under MPL-2.0.

template<typename T, unsigned N>
class Vector
{
  public:
    Vector() {}
    unsigned length() const { return 0; }
};

class TokenStreamShared
{
};

template<typename CharT, class AnyCharsAccess>
class TokenStreamSpecific;

class TokenStreamAnyChars
  : public TokenStreamShared
{
  public:
    TokenStreamAnyChars() {}
};

template<typename CharT>
class SourceUnits
{
  public:
    SourceUnits() {}

    bool atEnd() const { return true; }
    unsigned offset() const { return 0; }
    bool matchCodeUnit(CharT c) { return true; }
};

class TokenStreamCharsShared
{
    using CharBuffer = Vector<char16_t, 32>;

  protected:
    CharBuffer charBuffer;

  protected:
    explicit TokenStreamCharsShared() {}
};

template<typename CharT>
class TokenStreamCharsBase
  : public TokenStreamCharsShared
{
  public:
    TokenStreamCharsBase()
      : TokenStreamCharsShared(), sourceUnits()
    {}

    using SourceUnits = ::SourceUnits<CharT>;

    bool matchCodeUnit(int expect) { return true; }

  protected:
    SourceUnits sourceUnits;
};

template<typename CharT, class AnyCharsAccess>
class GeneralTokenStreamChars
  : public TokenStreamCharsBase<CharT>
{
    using CharsBase = TokenStreamCharsBase<CharT>;

  protected:
    using CharsBase::CharsBase;

    TokenStreamAnyChars& anyCharsAccess();
    const TokenStreamAnyChars& anyCharsAccess() const;
};

template<typename CharT, class AnyCharsAccess> class TokenStreamChars;

template<class AnyCharsAccess>
class TokenStreamChars<char16_t, AnyCharsAccess>
  : public GeneralTokenStreamChars<char16_t, AnyCharsAccess>
{
  private:
    using CharsBase = TokenStreamCharsBase<char16_t>;
    using GeneralCharsBase = GeneralTokenStreamChars<char16_t, AnyCharsAccess>;
    using Self = TokenStreamChars<char16_t, AnyCharsAccess>;

  protected:
    using GeneralCharsBase::anyCharsAccess;
    using CharsBase::sourceUnits;

    using typename GeneralCharsBase::SourceUnits;

  protected:
    using GeneralCharsBase::GeneralCharsBase;

    bool getFullAsciiCodePoint(int lead, int* codePoint) {
        if (lead == '\r') {
            bool isAtEnd = sourceUnits.atEnd();
            if (!isAtEnd)
                sourceUnits.matchCodeUnit('\n');
        } else if (lead != '\n') {
            *codePoint = lead;
            return true;
        }

        *codePoint = '\n';
        return true;
    }
};

template<typename CharT, class AnyCharsAccess>
class TokenStreamSpecific
  : public TokenStreamChars<CharT, AnyCharsAccess>,
    public TokenStreamShared
{
  public:
    using CharsBase = TokenStreamCharsBase<CharT>;
    using GeneralCharsBase = GeneralTokenStreamChars<CharT, AnyCharsAccess>;
    using SpecializedCharsBase = TokenStreamChars<CharT, AnyCharsAccess>;

  public:
    using GeneralCharsBase::anyCharsAccess;

  private:
    using typename CharsBase::SourceUnits;

  private:
    using TokenStreamCharsShared::charBuffer;
    using CharsBase::sourceUnits;

  public:
    TokenStreamSpecific()
      : SpecializedCharsBase()
    {}

  public:
    bool advance(unsigned position) {
        bool t = charBuffer.length() + 1 > 0;
        auto offs = sourceUnits.offset();
        return t && offs > 0;
    }
};

class TokenStreamAnyCharsAccess
{
};

class TokenStream final
  : public TokenStreamAnyChars,
    public TokenStreamSpecific<char16_t, TokenStreamAnyCharsAccess>
{
    using CharT = char16_t;

  public:
    TokenStream()
    : TokenStreamAnyChars(),
      TokenStreamSpecific<CharT, TokenStreamAnyCharsAccess>()
    {}
};

class SyntaxParseHandler {};

class ParserBase
{
  public:
    TokenStreamAnyChars anyChars;
};

template<class ParseHandler, typename CharT> class GeneralParser;

template <class ParseHandler>
class PerHandlerParser : public ParserBase
{
};

template<class Parser>
class ParserAnyCharsAccess
{
};

template <class ParseHandler, typename CharT>
class Parser;

template <class ParseHandler, typename CharT>
class GeneralParser
  : public PerHandlerParser<ParseHandler>
{
  public:
    TokenStreamSpecific<CharT, ParserAnyCharsAccess<GeneralParser>> tokenStream;

  public:
    GeneralParser();
};


template class TokenStreamCharsBase<char16_t>;

template class TokenStreamChars<char16_t, TokenStreamAnyCharsAccess>;

template class
TokenStreamChars<char16_t, ParserAnyCharsAccess<GeneralParser<SyntaxParseHandler, char16_t>>>;

template class
TokenStreamSpecific<char16_t, ParserAnyCharsAccess<GeneralParser<SyntaxParseHandler, char16_t>>>;
