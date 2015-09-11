// PR c++/51079

#if __cplusplus > 199711L
struct C1
{
  template <class T>
  operator T() = delete;	// { dg-message "declared here" "" { target c++11 } }
  operator bool() { return false; }
} c1;

int ic1 = c1;			// { dg-error "deleted" "" { target c++11 } }
int ac1 = c1 + c1;		// { dg-error "deleted" "" { target c++11 } }
#endif

struct C2
{
private:
  template <class T>
  operator T();			// { dg-message "private" }
public:
  operator bool() { return false; }
} c2;

int ic2 = c2;			// { dg-error "" }
int ac2 = c2 + c2;		// { dg-error "" }
