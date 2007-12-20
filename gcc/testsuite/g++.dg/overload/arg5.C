// PR c++/34111

class QChar
{
};
struct QString
{
  QString(QChar);
};
struct QPainter
{
  void drawText (int x, int y, const QString &);
};

  class KHEChar:public QChar
  {
  public:KHEChar (QChar C);
  };

void
drawByte (QPainter * P, char, KHEChar B)
{
  P->drawText (0, 0, B);
}
