// PR middle-end/51647
// { dg-do compile }
// { dg-options "-Wall" }

enum PropertyAttributes { NONE = 1 };
enum PropertyType { NORMAL = 0, FIELD = 1 };
class LookupResult;

template <typename T>
struct Handle
{
  inline explicit Handle (T *obj) __attribute__ ((always_inline)) {}
  inline T *operator-> () const __attribute__ ((always_inline)) { return 0; }
};

struct JSObject
{
  bool IsGlobalObject () { return false; }
};

struct Isolate
{
  LookupResult *top_lookup_result () { return 0; }
};

struct LookupResult
{
  explicit LookupResult (Isolate *isolate) {}
  JSObject *holder () { return 0; }
  PropertyType type () { return NORMAL; }
};

int
test (LookupResult *lookup)
{
  Handle <JSObject> holder (lookup->holder ());
  switch (lookup->type ())
    {
    case NORMAL:
      if (holder->IsGlobalObject ())
	return 2;
      else
	return 3;
      break;
    default:
      return 4;
    }
}	// { dg-bogus "control reaches end of non-void function" }
