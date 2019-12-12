// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }
// { dg-additional-options "-mfma" { target x86_64-*-* i?86-*-* } }

struct ef
{
  ef (double xy) : m6 (xy)
  {
  }

  ~ef ()
    {
    }

  double m6;
};

ef
operator- (ef &db, ef oa)
{
  return db.m6 - oa.m6;
}

ef
vu (ef &db)
{
  return db - ef (db.m6 * 1.1);
}
