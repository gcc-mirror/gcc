// PR c++/19148

struct QChar {
  QChar (char c);
  QChar (const QChar &);
  unsigned short ucs;
};

void f(QChar *uc, unsigned short ch, QChar replacement)
{
  *uc++ = ((ch) ? QChar((1)) : replacement);
}
