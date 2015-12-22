// { dg-do compile }
// { dg-options "-fgnu-tm" }

void f()
{
  __transaction_relaxed {
    try { throw 42; }
    catch (...) { }
  }
}

