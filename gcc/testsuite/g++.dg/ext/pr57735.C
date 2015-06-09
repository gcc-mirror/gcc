/* { dg-do compile { target arm*-*-* } } */
/* { dg-require-effective-target arm_arch_v5te_ok } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } {"-mfloat-abi=soft" } } */
/* { dg-options "-march=armv5te -marm  -mtune=xscale -mfloat-abi=soft -O1" } */

typedef unsigned int size_t;
__extension__
typedef long long int int64_t;
namespace WTF {
    template<typename T> class RefPtr {
    public:
        inline T* operator->() const { return m_ptr; }
        T* m_ptr;
    };
}
using WTF::RefPtr;
namespace JSC {
    class ExecState;
    class JSString;
    typedef int64_t EncodedJSValue;
    class JSValue {
    public:
        static EncodedJSValue encode(JSValue);
        JSString* toString(ExecState*) const;
    };
}
typedef unsigned char LChar;
    typedef short unsigned int UChar;
namespace WTF {
    template<typename T, size_t inlineCapacity = 0>
    class Vector {
    public:
        template<typename U> bool tryAppend(const U*, size_t);
    };
}
using WTF::Vector;
namespace WTF {
template<typename CharType> inline bool isASCIIDigit(CharType c)
{
}
template<typename CharType> inline bool isASCIIHexDigit(CharType c)
{
    return isASCIIDigit(c) || ((c | 0x20) >= 'a' && (c | 0x20) <= 'f');
}
    class StringImpl;
}
using WTF::StringImpl;
namespace WTF {
class StringImpl {
public:
    unsigned length() const { return m_length; }
    unsigned m_length;
};
}
namespace JSC {
    class Register {
    };
class UString {
public:
    unsigned length() const
    {
        return m_impl->length();
    }
    const LChar* characters8() const
    {
    }
    RefPtr<StringImpl> m_impl;
};
    class ExecState : private Register {
    public:
        JSValue argument(size_t argument)
        {
        }
    };
    class JSCell {
    };
    class JSString : public JSCell {
    public:
        const UString& value(ExecState*) const;
    };
class JSStringBuilder {
public:
    void append(const UChar u)
    {
        m_okay &= buffer16.tryAppend(&u, 1);
    }
    Vector<UChar, 64> buffer16;
    bool m_okay;
};
template <typename T>
class Lexer {
public:
    static unsigned char convertHex(int c1, int c2);
};
}
namespace WTF {
namespace Unicode {
    int UTF8SequenceLength(char);
    int decodeUTF8Sequence(const char*);
}
}
using namespace WTF;
using namespace Unicode;
namespace JSC {
template <typename CharType>
static JSValue decode(ExecState* exec, const CharType* characters, int length, const char* doNotUnescape, bool strict)
{
    JSStringBuilder builder;
    int k = 0;
    UChar u = 0;
    while (k < length) {
        const CharType* p = characters + k;
        CharType c = *p;
        if (c == '%') {
            int charLen = 0;
            if (k <= length - 3 && isASCIIHexDigit(p[1]) && isASCIIHexDigit(p[2])) {
                const char b0 = Lexer<CharType>::convertHex(p[1], p[2]);
                const int sequenceLen = UTF8SequenceLength(b0);
                if (sequenceLen && k <= length - sequenceLen * 3) {
                    charLen = sequenceLen * 3;
                    char sequence[5];
                    if (charLen != 0) {
                        const int character = decodeUTF8Sequence(sequence);
                        if (character < 0 || character >= 0x110000)
                            charLen = 0;
                        else if (character >= 0x10000) {
                            builder.append(static_cast<UChar>(0xD800 | ((character - 0x10000) >> 10)));
                        } else
                            u = static_cast<UChar>(character);
                    }
                }
            }
        }
    }
}
static JSValue decode(ExecState* exec, const char* doNotUnescape, bool strict)
{
    UString str = exec->argument(0).toString(exec)->value(exec);
        return decode(exec, str.characters8(), str.length(), doNotUnescape, strict);
}
EncodedJSValue globalFuncDecodeURI(ExecState* exec)
{
    static const char do_not_unescape_when_decoding_URI[] =
        "#$&+,/:;=?@";
    return JSValue::encode(decode(exec, do_not_unescape_when_decoding_URI, true));
}
}
