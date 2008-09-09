// PR c++/37417
// Testcase by Martin Michlmayr <tbm@cyrius.com>
// { dg-do compile }
// { dg-options "-O" }

void
test (int a)
{
  new (char[a]);
}
