// PR c++/9342
// We were failing to wrap def().user in a TARGET_EXPR, so copying it
// into the reference temp used a bitwise copy.

struct QString
{
    QString( const QString & );
    QString &operator=( const QString & );
};
struct ServerSettings
{
    QString user;
};
extern ServerSettings def();
extern void arg( const QString& a );
void startSession( bool b, QString s )
{
    arg (b ? def().user : s);
}
