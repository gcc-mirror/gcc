/* { dg-do compile } */
/* { dg-options "-O" } */

struct iterator{};
struct ByteIterator : iterator
{
        ByteIterator (){}
        int a[1024];
};
inline ByteIterator f ()
{
        return  ByteIterator ();
}
class ConfLexerCore
{
        ConfLexerCore ();
        ByteIterator m_matchStart;
};
ConfLexerCore::ConfLexerCore ()
: m_matchStart (f ())
{ }

