// Build don't link: 
typedef unsigned long   Xv_opaque;

class DynaString
{
public:
    DynaString();
    DynaString( const DynaString& dynaStr );
    DynaString( const long n );
    ~DynaString();

    int operator ==( const char* const string ) const;
};

class DDE_Defaults
{
public:
        DynaString      GetHost();
        DynaString      GetService();
        DynaString      GetDatabase();
};

extern DDE_Defaults* ddeDefaults;

void
f()
{
        DynaString tempHost, tempService, tempDatabase;
        if(     (tempHost = ddeDefaults->GetHost()) == 0
                || (tempService = ddeDefaults->GetService()) == 0
                || (tempDatabase = ddeDefaults->GetDatabase()) == 0
        )
        {
        }
}

