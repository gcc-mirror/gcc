// Build don't link: 
// GROUPS passed ARM-compliance
// unsorted.2 file
// Message-Id: <BpBu19.GrF@math.waterloo.edu>
// Date: Thu, 4 Jun 1992 15:07:56 GMT
// Subject: access control
// From: gjditchf@plg.waterloo.edu (Glen Ditchfield)


class X {
  private:
    enum E1 {a1, b1}; // ERROR - private
  public:
    enum E2 {a2, b2};
    };

void h(X* p) {
    X::E2 e2;
    int x2 = X::a2;

    X::E1 e1;                   // ERROR - within this context
    int x1 = X::a1;             // ERROR - Should be rejected, and is.
    }

