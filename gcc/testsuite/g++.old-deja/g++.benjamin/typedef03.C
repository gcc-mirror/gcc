// Build don't link:
//980526 bkoz
// reduced testcase for 980511 brendan qt bug


class QTextStream				 
{
public:
    QTextStream();
    virtual ~QTextStream();

    enum {
	skipws	  = 0x0001,			 
	left	  = 0x0002,			 
	right	  = 0x0004,			 
	internal  = 0x0008,			 
	bin	  = 0x0010,			 
	oct	  = 0x0020,			 
	dec	  = 0x0040,			 
	hex	  = 0x0080,			 
	showbase  = 0x0100,			 
	showpoint = 0x0200,			 
	uppercase = 0x0400,			 
	showpos	  = 0x0800,			 
	scientific= 0x1000,			 
	fixed	  = 0x2000			 
    };

    static const int basefield;			 
    static const int adjustfield;	
};

typedef QTextStream QTS;
const int QTS::basefield   = (QTS::bin | QTS::dec | QTS::hex) ;
const int QTS::adjustfield = QTS::left | QTS::right | QTS::internal;
#if 0
#define QTS QTextStream
const int QTS::basefield   = (QTS::bin | QTS::dec | QTS::hex) ;
const int QTS::adjustfield = QTS::left | QTS::right | QTS::internal;
#endif




