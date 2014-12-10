#define _ASSERTION(expr, message) { if (!(expr)) __builtin_abort (); } (void)0

typedef unsigned int EnumStatusCode;

class StatusCode
{
public:
	static const EnumStatusCode ERROR = 0x8000;
	static const EnumStatusCode SUCCESS = 0x0000;
	static bool isSUCCEEDED (EnumStatusCode res) { return (res == SUCCESS); }
};

class LocalizedTextStruct
{
public:
	LocalizedTextStruct () {}
	LocalizedTextStruct (const char *val)
	{
		__builtin_strcpy (t, val);
	}
	char *getT () { return t; }
private:
	char t[99];
};

typedef union tagValueUnion
{
	LocalizedTextStruct* LocalizedText;
} ValueStructUnion;

typedef struct ValueStruct
{
	unsigned char arrayType;
	unsigned short dataType;
	ValueStructUnion value;
} ValueStruct;

class LocalizedText
{
public:
	virtual LocalizedTextStruct* getInternHandle ();
private:
	LocalizedTextStruct t;
};

class ValueHelper
{
public:
	static EnumStatusCode getLocalizedText (const ValueStruct* pValueStruct, LocalizedText& target);
	static LocalizedText getLocalizedText (const ValueStruct* pValueStruct);
};

EnumStatusCode LocalizedTextSet (LocalizedTextStruct* pTarget, LocalizedTextStruct* pSource);
