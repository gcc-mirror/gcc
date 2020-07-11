// PR target/38287
// { dg-do run }
// { dg-options "-O2 -mcpu=v8 -fPIC" { target { { sparc*-*-* } && { ilp32 && fpic } } } }

#include <cstdlib>

typedef unsigned uint32_t __attribute__((mode (__SI__)));

class QTime
{
public:
    explicit QTime(int ms = 0) : ds(ms) {}
    static QTime currentTime() { return QTime(); }
    QTime addMSecs(int ms) const;
    int msecs() const { return ds; }
private:
    unsigned ds;
};

static const uint32_t MSECS_PER_DAY = 86400000;

QTime QTime::addMSecs(int ms) const
{
    QTime t;
    if ( ms < 0 ) {
        // % not well-defined for -ve, but / is.
        int negdays = (MSECS_PER_DAY-ms) / MSECS_PER_DAY;
        t.ds = ((int)ds + ms + negdays*MSECS_PER_DAY)
                % MSECS_PER_DAY;
    } else {
        t.ds = ((int)ds + ms) % MSECS_PER_DAY;
    }
    return t;
}

int main()
{
  if (QTime(1).addMSecs(1).msecs() != 2)
    abort ();
  return 0;
}
