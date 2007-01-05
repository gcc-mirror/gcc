/* { dg-do compile } */
/* { dg-options "-O3" } */

struct QDateTime
{
    QDateTime addSecs( int secs ) const;
    int t;
};
QDateTime gridToDate(long x)
{
  QDateTime date;
  date = date.addSecs(1);
  return date;
}
void whatsOnAt(long x, long y)
{
  gridToDate(x);
}

