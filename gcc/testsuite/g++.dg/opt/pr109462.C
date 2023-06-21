// PR tree-optimization/109462
// { dg-do run { target c++11 } }
// { dg-options "-O2" }

struct A {
  A (const char *);
  A (const char *, int);
  bool empty ();
  int size ();
  bool equals (A);
  A trim (char);
  A trim ();
};
[[gnu::noipa]] A::A (const char *) {}
[[gnu::noipa]] A::A (const char *, int) { __builtin_abort (); }
[[gnu::noipa]] bool A::empty () { __builtin_abort (); }
[[gnu::noipa]] int A::size () { __builtin_abort (); }
[[gnu::noipa]] bool A::equals (A) { return true; }
[[gnu::noipa]] A A::trim (char) { __builtin_abort (); }
[[gnu::noipa]] A A::trim () { __builtin_abort (); }

enum B { raw_identifier = 6, l_paren = 21, r_paren = 22 };
[[gnu::noipa]] bool isAnyIdentifier (B) { return true; }
[[gnu::noipa]] bool isStringLiteral (B) { __builtin_abort (); }

struct C {
  B c;
  B getKind () { return c; }
  bool is (B x) { return c == x; }
  unsigned getLength () { __builtin_abort (); }
  A getRawIdentifier () {
    A x ("");
    c == raw_identifier ? void () : __builtin_abort ();
    return x;
  }
  const char *getLiteralData ();
};
[[gnu::noipa]] const char *C::getLiteralData () { __builtin_abort (); }

struct D {
  D ();
  bool LexFromRawLexer (C &);
};
[[gnu::noipa]] D::D () {}
[[gnu::noipa]] bool D::LexFromRawLexer (C &t) {
  static int cnt;
  C tok[] = { { raw_identifier }, { l_paren }, { raw_identifier }, { r_paren } };
  t = tok[cnt++];
  return false;
}

bool ok = false;
[[gnu::noipa]] void reportEmptyContextError ()
{
  ok = true;
}

[[gnu::noipa]] void
VisitObjCMessageExpr ()
{
  D TheLexer;
  C I;
  C Result;
  int p_count = 0;
  while (!TheLexer.LexFromRawLexer (I)) {
    if (I.getKind () == l_paren)
      ++p_count;
    if (I.getKind () == r_paren) {
      if (p_count == 1)
        break;
      --p_count;
    }
    Result = I;
  }
  if (isAnyIdentifier (Result.getKind ())) {
    if (Result.getRawIdentifier ().equals ("nil")) {
      reportEmptyContextError ();
      return;
    }
  }
  if (!isStringLiteral (Result.getKind ()))
    return;
  A Comment = A (Result.getLiteralData (), Result.getLength ()).trim ('"');
  if ((Comment.trim ().size () == 0 && Comment.size () > 0) || Comment.empty ())
    reportEmptyContextError ();
}

int
main ()
{
  VisitObjCMessageExpr ();
  if (!ok)
    __builtin_abort ();
}
