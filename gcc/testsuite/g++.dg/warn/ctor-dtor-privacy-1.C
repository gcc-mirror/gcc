// { dg-options "-Wctor-dtor-privacy" }

struct C {                      // { dg-warning "" }
   static bool result;
private:
   static bool check();
};

bool C::result = check();
