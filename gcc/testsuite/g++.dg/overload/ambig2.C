// Bug: g++ thought that the QChar overload was a viable but ambiguous
// candidate.  It isn't viable, because there's no conversion from const
// char * to QChar.

class QChar {
public:
    QChar( char c );
    QChar( unsigned char c );
};

class QString
{
public:
    QString( const char *str );                 // deep copy

    QString    &insert( unsigned int index, const QString & );
    QString    &insert( unsigned int index, QChar );
    QString    &prepend( const char* );
};

inline QString &QString::prepend( const char* s )
{ return insert(0,s); }
