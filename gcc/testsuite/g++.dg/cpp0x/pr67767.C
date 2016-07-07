// PR c++/67767
// { dg-do compile { target c++11 } }
// { dg-options "-Wsuggest-attribute=noreturn" }

void foo [[gnu::cold, gnu::noreturn]] ();

void foo ()	// { dg-bogus "function might be candidate for attribute" }
{
  throw 1;
}
