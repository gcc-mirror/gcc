// { dg-do assemble  }
// { dg-options "-Wshadow" }
// GROUPS passed shadow-warnings
// shadow file
// Message-Id: <9211061827.AA03517@harvey>
// From: Jeff Gehlhaar <jbg@qualcomm.com>
// Subject: GCC Bug..
// Date: Fri, 6 Nov 1992 10:27:10 -0700

class Klasse
{
public:
        Klasse(void);           // constructor
        int Shadow(void);       // member function
private:
        long value;
};

Klasse::Klasse(void)
{
        value = 0;
}

static inline unsigned char
Function(int Shadow)
{
        return 0;
}
