// PR c++/71909
// { dg-do compile }

struct S
{
  S () try : m (0) {}
  catch (...) {}
  void foo () try {}
  catch (int) {}
  catch (...) {}
  int m;
};

struct T
{
  T () : m (0) {}
  catch (...) {}	// { dg-error "expected unqualified-id before" }
  void foo () {}
  catch (int) {}	// { dg-error "expected unqualified-id before" }
  catch (...) {}	// { dg-error "expected unqualified-id before" }
  int m;
};
