typedef unsigned short ushort;
class QChar {
public:
    QChar( const QChar& c );
    ushort ucs;
};
inline QChar::QChar( const QChar& c ) : ucs( c.ucs ) { };
class QString { };
class KoAutoFormat {
public:
    struct TypographicQuotes     { QChar begin, end; };
    TypographicQuotes getConfigTypographicDoubleQuotes() const     {
        return m_typographicDoubleQuotes;
    }
    TypographicQuotes m_typographicDoubleQuotes;
};
class KoAutoFormatDia {
    QChar oDoubleBegin, oDoubleEnd;
    KoAutoFormat * m_docAutoFormat;
    bool noSignal;
    void changeAutoformatLanguage(void);
};
void KoAutoFormatDia::changeAutoformatLanguage(void)
{
  oDoubleEnd= m_docAutoFormat->getConfigTypographicDoubleQuotes().end;
}
